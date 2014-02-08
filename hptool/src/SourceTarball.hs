module SourceTarball where

import Control.Monad (forM_)
import Development.Shake
import Development.Shake.FilePath

import Config
import Dirs
import Paths
import PlatformDB
import Utils

sourceTarballRules :: FilePath -> Rules ()
sourceTarballRules srcTarFile = do
    packageListRule listCore corePackages
    packageListRule listSource platformPackages

    srcTarFile *> \out -> do
        hpRelease <- askHp
        tarFileAction out hpRelease
  where
    packageListRule target pkgFn =
        target *> \out -> do
            hpRelease <- askHp
            let pkgs = map includeToPackage $ pkgFn hpRelease
            writeFileLinesChanged out (map show pkgs)


tarFileAction :: FilePath -> Release -> Action ()
tarFileAction out hpRelease = do
    need $ map (dir . packageSourceDir) sources ++ lists

    removeDirectoryRecursive topDir
    makeDirectory topDir
    makeDirectory etcDir
    makeDirectory packagesDir

    forM_ sources $ \sPkg ->
        command_ [] "cp"
            [ "-pR"
            , packageSourceDir sPkg
            , hpSourcePackageDir hp sPkg]

    forM_ lists $ \l -> do
        let dest = etcDir </> takeFileName l
        putLoud $ "copyFile' " ++ l ++ " " ++ dest
        copyFile' l dest

    command_ [Cwd upDir]
        "tar" ["czf", out ® upDir, takeFileName topDir]
  where
    hp = releaseToHp hpRelease

    upDir = takeDirectory topDir
    topDir = hpSourceDir hp
    etcDir = hpSourceEtcDir hp
    packagesDir = hpSourcePackagesDir hp

    lists = [listBuild, listCore, listSource]

    sources = map includeToPackage $ platformPackages hpRelease
