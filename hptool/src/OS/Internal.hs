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

        -- | Where each package is installed
    ,   osPackageTargetDir :: forall p. (PackagePattern p) => p -> FilePath

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

        -- | Additional switches/options for "ghc-pkg"
        -- querying the 'haddock-html' field with ghc-pkg.
    ,   osGhcPkgHtmlFieldExtras :: [String]

        -- | If needed by the platform, make needed changes to the path
        -- which will be used for the relative urls for platform packages
        -- when referenced by other packages.  The first argument is the
        -- base path, to which the 2nd argument should be made relative
        -- (after munging).
    ,   osPlatformPkgPathMunge :: FilePath -> HaddockPkgLoc -> HaddockPkgLoc

        -- | If needed by the platform, make needed changes to the path
        -- which will be used for the relative urls for GHC packages when
        -- referenced by other packages.  The first argument is the
        -- base path, to which the 2nd argument should be made relative
        -- (after munging).
    ,   osGhcPkgPathMunge :: FilePath -> HaddockPkgLoc -> HaddockPkgLoc

        -- | Path, relative to targetDir, expanded for the given package,
        -- where that package's html docs are installed.
    ,   osPkgHtmlDir :: Package -> FilePath

        -- | Extra actions run after haddock has built the master doc
    ,   osDocAction :: Action ()

        -- | Final, OS specific, product. Usually a tarball or installer.
        -- Should be located in productDir
    ,   osProduct :: FilePath

        -- | Rules for building the product and anything from osTargetExtraNeeds
    ,   osRules :: Release -> BuildConfig -> Rules ()

        -- | Extra arguments that are needed for "cabal configure"
    ,   osPackageConfigureExtraArgs :: Package -> [String]
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
    osGhcPkgHtmlFieldExtras = []
    osPlatformPkgPathMunge = flip const
    osGhcPkgPathMunge = flip const
    osPkgHtmlDir pkg = osPackageTargetDir pkg </> "doc" </> "html"
    osDocAction = return ()
    osProduct = productDir </> "generic.tar.gz"
    osRules _hpRelease _bc =
        osProduct %> \out -> do
            need [phonyTargetDir, vdir ghcVirtualTarget]
            command_ [Cwd buildRoot]
                "tar" ["czf", out `relativeToDir` buildRoot, targetDir `relativeToDir` buildRoot]

    osPackageConfigureExtraArgs _pkg = []
