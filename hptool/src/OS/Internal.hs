{-# LANGUAGE Rank2Types, RecordWildCards #-}

module OS.Internal
    ( OS(..)
    , genericOS
    )
  where

import Data.Version (showVersion)
import Development.Shake
import Development.Shake.FilePath

import Dirs
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

        -- | Platform-specific actions needed to finish the ghc-bindist/local
        -- install step (and before any HP packages are built)
    ,   osGhcLocalInstall :: GhcInstall

        -- | Platform-specific actions needed to finish the target-ghc
        -- install step
    ,   osGhcTargetInstall :: GhcInstall

        -- | Formats a path suitable for the native cabal's "--prefix" option
    ,   osToCabalPrefix :: FilePath -> FilePath

        -- | Where each package is installed
    ,   osPackageTargetDir :: (PackagePattern p) => p -> FilePath

        -- | Set True if GHC in this build supports creating shared libs
    ,   osDoShared :: Bool

        -- | Any action to take, after creation of the package conf file,
        -- before the package's haddock files are created.
    ,   osPackagePostRegister :: Package -> Action ()

        -- | Extra action to install the package from the build dir to the
        -- target image.
    ,   osPackageInstallAction :: Package -> Action ()

        -- | Extra actions run after GHC and the packages have been assembled
        -- into the target image.
    ,   osTargetAction :: Action ()

        -- | Directory relative to ghc install for package.conf.d
        -- (e.g., "lib/7.8.2/package.conf.d")
    ,   osGhcDbDir :: FilePath

        -- | Extra actions run after haddock has built the master doc
    ,   osDocAction :: Action ()

        -- | Final, OS specific, product. Usually a tarball or installer.
        -- Should be located in productDir
    ,   osProduct :: FilePath

        -- | Rules for building the product and anything from osTargetExtraNeeds
    ,   osRules :: Release -> BuildConfig -> Rules ()
    }


genericOS :: BuildConfig -> OS
genericOS BuildConfig{..} = OS{..}
  where
    HpVersion{..} = bcHpVersion
    GhcVersion{..} = bcGhcVersion
    osHpPrefix = "/usr/local/haskell-platform" </> showVersion hpVersion
    osGhcPrefix = "/usr/local/ghc" </> showVersion ghcVersion
    osGhcLocalInstall = GhcInstallConfigure
    osGhcTargetInstall = GhcInstallConfigure
    osToCabalPrefix = id
    osPackageTargetDir p = osHpPrefix </> "lib" </> packagePattern p
    osDoShared = True
    osPackagePostRegister _ = return ()
    osPackageInstallAction p = do
        let confFile = packageTargetConf p
        let regDir = targetDir </+> osHpPrefix </> "etc" </> "registrations"
        let regFile = regDir </> show p
        makeDirectory regDir
        hasReg <- doesFileExist confFile
        if hasReg
            then command_ [] "cp" [confFile, regFile]
            else command_ [] "rm" ["-f", regFile]
    osTargetAction = return ()
    osGhcDbDir = "lib" </> show bcGhcVersion </> "package.conf.d"
    osDocAction = return ()
    osProduct = productDir </> "generic.tar.gz"
    osRules _hpRelease _bc =
        osProduct *> \out -> do
            need [targetDir, vdir ghcVirtualTarget]
            command_ [Cwd buildRoot]
                "tar" ["czf", out ® buildRoot, targetDir ® buildRoot]


