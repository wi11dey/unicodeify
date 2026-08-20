{-# OPTIONS_GHC -Wno-x-partial #-}

module Language.Haskell.Tokens where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Text (Text)
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
import System.IO
import Text.Printf
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Utils

unicodeSyntax ∷ Token → Maybe String
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

locateChars ∷ FilePath → Text → [(SrcLoc, Char)]
locateChars filename text = do
  (lineNumber,   lineText) ← zip [1..] $ Text.lines text
  (columnNumber, char)     ← zip [1..] $ Text.unpack lineText ++ "\n"
  return (SrcLoc filename lineNumber columnNumber, char)

warnOnNonUnicode ∷ Bool → (SrcSpan, String) → IO ()
warnOnNonUnicode throw (SrcSpan {..}, replacement) = do
  source ← Text.readFile srcSpanFilename
  let startLoc = SrcLoc srcSpanFilename srcSpanStartLine srcSpanStartColumn
      endLoc   = SrcLoc srcSpanFilename srcSpanEndLine   srcSpanEndColumn
      withLocs = locateChars srcSpanFilename source
      srcSpan = map snd $ filter (\(loc, _) → startLoc <= loc && loc < endLoc) withLocs
  if srcSpan == replacement
  then return ()
  else (if throw then fail else hPutStrLn stderr) $ printf "Non-UnicodeSyntax at %s:%d,%d: %s"
    srcSpanFilename
    srcSpanStartLine
    srcSpanStartColumn
    srcSpan

replaceInPlace ∷ (SrcSpan, String) → IO ()
replaceInPlace (SrcSpan {..}, replacement) = do
  source ← Text.readFile srcSpanFilename
  let startLoc = SrcLoc srcSpanFilename srcSpanStartLine srcSpanStartColumn
      endLoc   = SrcLoc srcSpanFilename srcSpanEndLine   srcSpanEndColumn
      withLocs = locateChars srcSpanFilename source
  Text.writeFile srcSpanFilename $
    Text.pack $
    map snd (takeWhile ((< startLoc) . fst) withLocs) ++ replacement ++ map snd (dropWhile ((< endLoc) . fst) withLocs)

findReplaceableSpans ∷ (Token → Maybe String) → [Extension] → FilePath → IO [(SrcSpan, String)]
findReplaceableSpans substitution extensions file = do
  tokens ← tokenize extensions file
  return do
    Loc {..} ← tokens
    replacement ← maybeToList $ substitution unLoc
    return (loc, replacement)

replaceAllInPackage ∷ PackageDescription → (Token → Maybe String) → IO ()
replaceAllInPackage package substitution = do
  packageFiles ← listPackageSources normal "." package knownSuffixHandlers
  replacements ← concat <$> (
    forM packageFiles $
    findReplaceableSpans substitution $
    enabledPackageExtensions package)
  sequence_ $ reverse $ map replaceInPlace replacements
