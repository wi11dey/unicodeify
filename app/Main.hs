import Data.List.NonEmpty
import Options.Applicative

data Options = Options
  { dryRun :: Bool
  , files :: NonEmpty FilePath
  }
  deriving Show

description :: InfoMod a
description = fullDesc <> progDesc "Enforce UnicodeSyntax usage in Haskell source. By default, runs on the Cabal project in the current working directory."

options :: Parser Options
options = Options <$>
  switch (long "dry-run" <> help "Fail with stderr if there is non-Unicode syntax") <*>
  (maybe (singleton ".") id . nonEmpty
    <$> many (argument str (metavar "FILES..." <> help "Files or directories of Cabal projects to process")))

main :: IO ()
main = do
  Options {..} <- execParser $ info (options <**> helper) description
  return ()
