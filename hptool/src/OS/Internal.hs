{-# LANGUAGE Rank2Types, RecordWildCards #-}

module OS.Internal
    ( OS(..)
    , genericOS
    )
  where

import Data.Version (showVersion)
import Development.Shake
import Development.Shake.FilePath

import Paths
import Types
import Utils

data OS = OS
    {
        -- These paths are all on the target machine.
        -- They should all be absolute paths.
        -- During the build, they will be made relative to targetDir

        -- | Where HP will be installed
        osHpPrefix :: FilePath

        -- | Where GHC will be installed
    ,   osGhcPrefix :: FilePath

        -- | Where each package is installed
    ,   osPackageTargetDir :: (PackagePattern p) => p -> FilePath

        -- | Extra action to install the package from the build dir to the
        -- target image.
    ,   osPackageInstallAction :: Package -> Action ()

        -- | Final, OS specific, product. Usually a tarball or installer.
        -- Should be located in productDir
    ,   osProduct :: FilePath

        -- | Action that builds the final product.
        -- The target will already have been depended on
    ,   osProductAction :: Action ()
    }


genericOS :: BuildConfig -> OS
genericOS BuildConfig{..} = OS{..}
  where
    HpVersion{..} = bcHpVersion
    GhcVersion{..} = bcGhcVersion
    osHpPrefix = "/usr/local/haskell-platform" </> showVersion hpVersion
    osGhcPrefix = "/usr/local/ghc" </> showVersion ghcVersion
    osPackageTargetDir p = osHpPrefix </> "lib" </> packagePattern p
    osPackageInstallAction p = do
        let confFile = packageTargetConf p
        let regDir = osHpPrefix </> "etc" </> "registrations"
        let regFile = regDir </> show p
        makeDirectory regDir
        hasReg <- doesFileExist confFile
        if hasReg
            then command_ [] "cp" [confFile, regFile]
            else command_ [] "rm" ["-f", regFile]
    osProduct = productDir </> "generic.tar.gz"
    osProductAction =
        command_ [Cwd buildRoot]
            "tar" ["czf", osProduct ® buildRoot, targetDir ® buildRoot]


