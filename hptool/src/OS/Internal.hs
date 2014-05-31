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

        -- | Where each package is installed
    ,   osPackageTargetDir :: (PackagePattern p) => p -> FilePath

        -- | Extra action to install the package from the build dir to the
        -- target image.
    ,   osPackageInstallAction :: Package -> Action ()

        -- | Extra actions run after GHC and the packages have been assembled
        -- into the target image.
    ,   osTargetAction :: Action ()

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
    osPackageTargetDir p = osHpPrefix </> "lib" </> packagePattern p
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
    osDocAction = return ()
    osProduct = productDir </> "generic.tar.gz"
    osRules _hpRelease _bc =
        osProduct */> \out ->
            command_ [Cwd buildRoot]
                "tar" ["czf", out ® buildRoot, targetDir ® buildRoot]


