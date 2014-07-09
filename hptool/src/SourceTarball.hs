module SourceTarball where

import Control.Monad (forM_)
import Development.Shake
import Development.Shake.FilePath

import Config
import Dirs
import LocalCommand
import Paths
import PlatformDB
import Templates
import Types
import Utils

sourceTarballRules :: FilePath -> Rules ()
sourceTarballRules srcTarFile = do
    packageListRule listCore corePackages
    packageListRule listSource platformPackages
    cabalFileRule

    srcTarFile *> \out -> do
        hpRelease <- askHpRelease
        tarFileAction out hpRelease
  where
    packageListRule target pkgFn =
        target *> \out -> do
            hpRelease <- askHpRelease
            let pkgs = pkgFn hpRelease
            writeFileLinesChanged out (map show pkgs)


tarFileAction :: FilePath -> Release -> Action ()
tarFileAction out hpRelease = do
    need $ concat
        [ map (dir . packageSourceDir) sources
        , lists
        , [hpCabalFile, dir ghcLocalDir]
        ]

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

    copyFile' hpCabalFile $ topDir </> takeFileName hpCabalFile

    localCommand' [Cwd hptoolSourceDir]
        "cabal" [ "sdist"
                , "--output-directory=" ++ hptoolDistDir ® hptoolSourceDir ]

    command_ [Cwd upDir]
        "tar" ["czf", out ® upDir, takeFileName topDir]
  where
    hp = relVersion hpRelease

    upDir = takeDirectory topDir
    topDir = hpSourceDir hp
    etcDir = hpSourceEtcDir hp
    packagesDir = hpSourcePackagesDir hp
    hptoolSourceDir = "hptool"
    hptoolDistDir = topDir </> "hptool"

    lists = [listBuild, listCore, listSource]

    sources = platformPackages hpRelease


cabalFileRule :: Rules ()
cabalFileRule =
    hpCabalFile *> \cFile -> do
        ctx <- releaseContext
        copyExpandedFile ctx cabalTemplate cFile
  where
    cabalTemplate = "hptool/templates/haskell-platform.cabal.mu" -- FIXME
