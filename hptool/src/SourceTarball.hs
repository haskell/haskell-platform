module SourceTarball where

import Control.Monad (forM_)
import Development.Shake
import Development.Shake.FilePath
import Prelude hiding ((*>))

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
        , topFiles
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

    forM_ topFiles $ \f -> copyFile' f $ topDir </> f

    localCommand' [Cwd hptoolSourceDir]
        "cabal" [ "sdist"
                , "--output-directory=" ++ hptoolDistDir ® hptoolSourceDir ]

    -- this is a hack because adding os-extras to hptool's .cabal file would
    -- be inordinately painful
    command_ [] "cp"
        [ "-pR"
        , hptoolSourceDir </> "os-extras"
        , hptoolDistDir
        ]

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
    topFiles =
        [ "LICENSE"
        , "platform.sh"
        , "README"
        , "windows-platform.sh"
        ]

    sources = platformPackages hpRelease


cabalFileRule :: Rules ()
cabalFileRule =
    hpCabalFile *> \cFile -> do
        ctx <- releaseContext
        let ctx' = ctx `ctxAppend` errorCtx
        copyExpandedFile ctx' cabalTemplate cFile
  where
    cabalTemplate = "hptool/templates/haskell-platform.cabal.mu" -- FIXME
