import Control.Monad
import Data.List (isSuffixOf)
import Data.List.NonEmpty
import Distribution.Simple.Errors
import Distribution.Simple.PackageDescription
import Distribution.Simple.Utils hiding (info)
import Distribution.Types.GenericPackageDescription
import Distribution.Verbosity
import Language.Haskell.Exts.Extension
import Options.Applicative
import System.Directory
import System.IO

data Options = Options
  { dryRun :: Bool
  , paths :: NonEmpty FilePath
  }
  deriving Show

description :: InfoMod a
description = fullDesc <> progDesc "Enforce UnicodeSyntax usage in Haskell source. By default, runs on the Cabal project in the current working directory."

options :: Parser Options
options = Options <$>
  switch (long "dry-run" <> help "Fail with stderr if there is non-Unicode syntax") <*>
  (maybe (singleton ".") id . nonEmpty
    <$> many (argument str (metavar "PATHS..." <> help "Files or directories of Cabal projects to process")))

main :: IO ()
main = do
  Options {..} <- execParser $ info (options <**> helper) description
  files <- concat <$> forM paths \path -> do
    isFile      <- doesFileExist      path
    isDirectory <- doesDirectoryExist path
    if ".cabal" `isSuffixOf` path
    then if not isFile then do
      hPutStrLn stderr $ path ++ " is not a Cabal file as was expected"
      return []
    else do
      package <- packageDescription <$> readGenericPackageDescription silent path
      return []
    else if isDirectory then do
      maybeCabalFile <- findPackageDesc path
      case maybeCabalFile of
        Left exception -> do
          hPutStrLn stderr $ "Couldn't find Cabal file in project " ++ path ++ ": " ++ exceptionMessage exception
          return []
        Right cabalFile -> do
          package <- packageDescription <$> readGenericPackageDescription silent cabalFile
          return []
    else if isFile then do
      return [(ghcDefault, path)]
    else do
      hPutStrLn stderr $ path ++ " does not exist"
      return []
  return ()
