module Utils where

import Distribution.Types.BuildInfo
import Distribution.Types.PackageDescription
import Language.Haskell.Exts.Extension
import qualified Language.Haskell.Extension as Cabal

enabledPackageExtensions ∷ PackageDescription → [Extension]
enabledPackageExtensions package = do
  BuildInfo {..} ← allBuildInfo package
  Cabal.EnableExtension extension ← defaultExtensions ++ otherExtensions
  return $ classifyExtension $ show extension
