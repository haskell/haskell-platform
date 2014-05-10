module Main where

import Control.Monad (forM_)
import Development.Shake
import Development.Shake.FilePath

import Config
import Dirs
import GhcDist
import HaddockMaster
import Package
import Paths
import PlatformDB
import Releases2014
import SourceTarball
import Types
import Target

main :: IO ()
main = shakeArgsWith opts [] main'
  where
    main' [] (tarfile:what) = return $ Just $ do
        allRules tarfile
        want $ if null what then ["build-all"] else what
    main' _ _ = usage

    usage = do
        putStrLn "usage: hptool [opts] <ghc-bindist.tar.bz> [target...]\n\
                 \  where target is one of:\n\
                 \    build-all           -- build everything (default)\n\
                 \    build-source        -- build the source tar ball\n\
                 \    build-target        -- build the target tree\n\
                 \    build-package-<pkg> -- build the package (name or name-ver)\n\
                 \    build-local         -- build the local GHC environment\n"
        return Nothing

    allRules tarfile = do
        buildConfig <- addConfigOracle hpRelease tarfile
        ghcDistRules
        packageRules
        targetRules buildConfig
        haddockMasterRules buildConfig
        sourceTarballRules srcTarFile
        buildRules hpRelease srcTarFile

    opts = shakeOptions

    hpRelease = hp2014_1_0_0
    hpFullName = show $ relVersion hpRelease
    srcTarFile = productDir </> hpFullName <.> "tgz"


buildRules :: Release -> FilePath -> Rules()
buildRules hpRelease srcTarFile = do
    "build-source" ~> need [srcTarFile]
    "build-target" ~> need [targetDir]
    "build-local" ~> need [dir ghcLocalDir]
    forM_ (platformPackages hpRelease) $ \pkg -> do
        let full = "build-package-" ++ show pkg
        let short = "build-package-" ++ pkgName pkg
        short ~> need [full]
        full ~> need [dir $ packageBuildDir pkg]
    "build-all" ~> do  -- separate need call so built in order
        need ["build-source"]
        need ["build-target"]
