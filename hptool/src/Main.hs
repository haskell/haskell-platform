module Main where

import Control.Monad (forM_)
import Data.Version (showVersion)
import Development.Shake
import Development.Shake.FilePath

import Config
import Dirs
import GhcDist
import Package
import Paths
import PlatformDB
import Releases2014
import SourceTarball
import Target

main :: IO ()
main = shakeArgsWith opts [] main'
  where
    main' [] (tarfile:what) = case wanted what of
        Nothing -> usage
        Just ws -> return $ Just $ doBuild tarfile ws
    main' _ _ = usage

    wanted [] = Just ["build-all"]
    wanted ["src"] = Just ["build-source"]
    wanted ["target"] = Just ["build-target"]
    wanted ["pkg", pkg] = Just ["build-package-" ++ pkg]
    wanted ["local"] = Just ["build-local"]
    wanted ["path", path] = Just [path]
    wanted _ = Nothing

    usage = do
        putStrLn "usage: hptool [opts] <ghc-bindist.tar.bz> [target]\n\
                 \  where target is one of:\n\
                 \    src           -- build the source tar ball\n\
                 \    target        -- build the target tree\n\
                 \    pkg <pkg>     -- build the package\n\
                 \    local         -- build the local GHC environment\n\
                 \    -nothing-     -- if empty, build it all\n"
        return Nothing


    doBuild tarfile ws = do
        addConfigOracle hpRelease tarfile
        ghcDistRules
        packageRules
        targetRules (relVersion hpRelease)
        sourceTarballRules srcTarFile
        buildRules hpRelease srcTarFile
        want ws

    opts = shakeOptions

    hpRelease = hp2014_1_0_0
    hpFullName = "haskell-platform-" ++ showVersion (relVersion hpRelease)
    srcTarFile = productDir </> hpFullName <.> "tgz"


buildRules :: Release -> FilePath -> Rules()
buildRules hpRelease srcTarFile = do
    "build-source" ~> need [srcTarFile]
    "build-target" ~> need [targetDir]
    "build-local" ~> need [dir ghcLocalDir]
    forM_ (map includeToPackage $ platformPackages hpRelease) $ \pkg -> do
        let full = "build-package-" ++ show pkg
        let short = "build-package-" ++ pkgName pkg
        short ~> need [full]
        full ~> need [dir $ packageBuildDir pkg]
    "build-all" ~> mapM_ (need . (:[])) ["build-source", "build-target"]
  where
