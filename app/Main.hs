import Control.Monad
import Data.List (isSuffixOf, nub)
import Data.List.NonEmpty hiding (reverse, nub)
import Data.Maybe
import Distribution.PackageDescription.Configuration
import Distribution.Simple.Errors
import Distribution.Simple.PackageDescription
import Distribution.Simple.PreProcess
import Distribution.Simple.SrcDist
import Distribution.Simple.Utils hiding (info)
import Distribution.Verbosity (silent)
import Language.Haskell.Exts.Extension
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO

import Language.Haskell.Tokens
import Utils

data Options = Options
  { dryRun ∷ Bool
  , paths ∷ NonEmpty FilePath
  }
  deriving Show

description ∷ InfoMod a
description = fullDesc <> progDesc "Enforce UnicodeSyntax usage in Haskell source. By default, runs on the Cabal project in the current working directory."

options ∷ Parser Options
options = Options <$>
  switch (long "dry-run" <> help "Fail with stderr if there is non-Unicode syntax") <*>
  (fromMaybe (singleton ".") .
   nonEmpty <$>
   many (argument str (metavar "PATHS..." <> help "Files or directories of Cabal projects to process")))

filesFromCabal ∷ FilePath → IO [([Extension], FilePath)]
filesFromCabal cabalFile = do
  package ← flattenPackageDescription <$> readGenericPackageDescription silent cabalFile
  packageFiles ← listPackageSources silent (takeDirectory cabalFile) package knownSuffixHandlers
  return $ (enabledPackageExtensions package,) <$> packageFiles

main ∷ IO ()
main = do
  Options {..} ← execParser $ info (options <**> helper) description
  files ← nub <$> concat <$> forM paths \path → do
    isFile      ← doesFileExist      path
    isDirectory ← doesDirectoryExist path
    if ".cabal" `isSuffixOf` path
    then if not isFile
    then do
      hPutStrLn stderr $ path ++ " is not a Cabal file as was expected"
      return []
    else filesFromCabal path
    else if isDirectory
    then findPackageDesc path >>= flip either filesFromCabal \exception → do
      hPutStrLn stderr $ "Couldn't find Cabal file in project " ++ path ++ ": " ++ exceptionMessage exception
      return []
    else if isFile
    then return [(ghcDefault, path)]
    else do
      hPutStrLn stderr $ path ++ " does not exist"
      return []
  replacements ← forM files $ uncurry $ findReplaceableSpans unicodeSyntax
  sequence_ $
    reverse $
    (if dryRun then warnOnNonUnicode False else replaceInPlace) <$>
    concat replacements
