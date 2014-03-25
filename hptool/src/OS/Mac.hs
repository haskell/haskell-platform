{-# LANGUAGE RecordWildCards #-}

module OS.Mac
    ( macOsFromConfig
    )
  where

import Data.Version
import Development.Shake
import Development.Shake.FilePath

import OS.Internal
import Paths
import Types
import Utils

macOsFromConfig :: BuildConfig -> OS
macOsFromConfig BuildConfig{..} = OS{..}
  where
    HpVersion{..} = bcHpVersion
    GhcVersion{..} = bcGhcVersion
    versionAndArch v = showVersion v ++ '-' : bcArch

    osHpPrefix = "/Library/Haskell"
                        </> versionAndArch hpVersion
    osGhcPrefix = "/Library/Frameworks/GHC.framework/Versions"
                        </> versionAndArch ghcVersion

    osPackageTargetDir p = osHpPrefix </> "lib" </> packagePattern p
    osPackageInstallAction p = do
        let confFile = packageTargetConf p
        let regDir = targetDir </+> osHpPrefix </> "lib" </> "registrations"
        let regFile = regDir </> show p
        makeDirectory regDir
        hasReg <- doesFileExist confFile
        if hasReg
            then command_ [] "cp" [confFile, regFile]
            else command_ [] "rm" ["-f", regFile]


