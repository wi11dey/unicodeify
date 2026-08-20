{-# OPTIONS_GHC -Wno-x-partial #-}

module Language.Haskell.Tokens where

import Control.Monad
import Data.List
import Data.Maybe
import Distribution.Simple.PreProcess
import Distribution.Simple.PreProcess.Unlit
import Distribution.Simple.SrcDist
import Distribution.Simple.Utils
import Distribution.Types.PackageDescription
import Distribution.Verbosity (normal)
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Utils

unicodeSyntax ∷ Token → Maybe Text
unicodeSyntax KW_Forall   = Just "∀"
unicodeSyntax DoubleColon = Just "∷"
unicodeSyntax RightArrow  = Just "→"
unicodeSyntax LeftArrow   = Just "←"
unicodeSyntax DoubleArrow = Just "⇒"
unicodeSyntax _           = Nothing

tokenize ∷ [Extension] → FilePath → IO [Loc Token]
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

indexChars ∷ FilePath → Text → [(SrcLoc, Char)]
indexChars filename text = do
  (lineNumber,   lineText) ← zip [1..] $ Text.lines text
  (columnNumber, char)     ← zip [1..] $ Text.unpack lineText
  return (SrcLoc filename lineNumber columnNumber, char)

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

findReplaceableSpans ∷ (Token → Maybe Text) → [Extension] → FilePath → IO [(SrcSpan, Text)]
findReplaceableSpans substitution extensions file = do
  tokens ← tokenize extensions file
  return do
    Loc {..} ← tokens
    replacement ← maybeToList $ substitution unLoc
    return (loc, replacement)

replaceAllInPackage ∷ PackageDescription → (Token → Maybe Text) → IO ()
replaceAllInPackage package substitution = do
  packageFiles ← listPackageSources normal "." package knownSuffixHandlers
  replacements ← concat <$> (
    forM packageFiles $
    findReplaceableSpans substitution $
    enabledPackageExtensions package)
  sequence_ $ reverse $ map replaceInPlace replacements
