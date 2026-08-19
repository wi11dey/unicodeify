{-# OPTIONS_GHC -Wno-x-partial #-}

module Language.Haskell.Tokens where

import Control.Monad
import Data.List
import Data.Maybe
import Distribution.Simple.PreProcess
import Distribution.Simple.PreProcess.Unlit
import Distribution.Simple.SrcDist
import Distribution.Simple.Utils
import Distribution.Types.BuildInfo
import Distribution.Types.PackageDescription
import Distribution.Verbosity
import Language.Haskell.Extension as Cabal hiding (classifyExtension, Extension)
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

unicodeSyntax ∷ Token → Maybe Text
unicodeSyntax KW_Forall   = Just "∀"
unicodeSyntax DoubleColon = Just "∷"
unicodeSyntax RightArrow  = Just "→"
unicodeSyntax LeftArrow   = Just "←"
unicodeSyntax DoubleArrow = Just "⇒"
unicodeSyntax _           = Nothing

tokenize :: [Extension] -> FilePath -> IO [Loc Token]
tokenize extensions parseFilename
  | ".lhs" `isSuffixOf` parseFilename = do
      source ← readFile parseFilename
      preprocessed ← either return (dieWithException normal) $ unlit parseFilename source
      return $
        fromParseResult $
        lexTokenStreamWithMode defaultParseMode { parseFilename, extensions } preprocessed
  | ".hs" `isSuffixOf` parseFilename = do
      source ← readFile parseFilename
      return $
        fromParseResult $
        lexTokenStreamWithMode defaultParseMode { parseFilename, extensions } source
  | otherwise = return []

replaceInPlace ∷ (SrcSpan, Text) → IO ()
replaceInPlace (SrcSpan { srcSpanFilename = filename
                 , srcSpanStartLine   = (subtract 1 → startLn)
                 , srcSpanStartColumn = (subtract 1 → startCol)
                 , srcSpanEndLine     = (subtract 1 → endLn)
                 , srcSpanEndColumn   = (subtract 1 → endCol)
                 },
          replacement) = do
  source ← Text.readFile filename
  let (prevLines, splitAt (endLn - startLn + 1) → (spanLines, nextLines)) =
        splitAt startLn $ Text.lines source
  Text.writeFile filename $
    Text.unlines $
    prevLines ++
      [ Text.take startCol (head spanLines) <>
        replacement <>
        Text.drop endCol (last spanLines)] ++
      nextLines

replaceAll ∷ PackageDescription -> (Token -> Maybe Text) → IO ()
replaceAll package substitution = do
  let extensions = do
        BuildInfo {..} ← allBuildInfo package
        Cabal.EnableExtension extension ← defaultExtensions ++ otherExtensions
        return $ classifyExtension $ show extension
  packageFiles ← listPackageSources normal "." package knownSuffixHandlers
  (concat → replacements) ← forM packageFiles \file → do
    tokens ← tokenize extensions file
    return do
      Loc {..} ← tokens
      replacement ← maybeToList $ substitution unLoc
      return (loc, replacement)
  sequence_ $ reverse $ map replaceInPlace replacements
