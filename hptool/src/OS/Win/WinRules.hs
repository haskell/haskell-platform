{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module OS.Win.WinRules
    ( pkgrootConfFixup
    , winGhcInstall
    , winRules
    )
  where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Development.Shake
import Development.Shake.FilePath ( (</>) )

import Dirs
import OS.Internal
import OS.Win.WinNsis
import OS.Win.WinPaths
import OS.Win.WinUtils
import Paths
import Types
import Utils


winRules :: FilePath -> Rules ()
winRules osProduct = do
    genNsisFiles osProduct
    copyInstExtras

winGhcInstall :: FilePath -> GhcInstallAction
winGhcInstall destDir bc distDir = do
    -- (will this cause some race conditions, removing vs populating?)
    command_ [] "mv" [distDir, destDir ]

    -- Install the GLUT components into destDir:
    --   lib, dll, ...
    let winGlutLibSrc = winExternalGlutLibDir bc </> winGlutLibFile
        winGlutDllSrc = winExternalGlutLibDir bc </> winGlutDllFile
        winGlutLibInstallDir = destDir </> winGlutLibDir
        winGlutDllInstallDir = destDir </> winGlutDllDir
    command_ [] "cp" [ "-p", winGlutLibSrc, winGlutLibInstallDir ]
    command_ [] "cp" [ "-p", winGlutDllSrc, winGlutDllInstallDir ]

    --   ... and the include files
    let setup = makeDirectory winGlutIncludeInstallDir
        winGlutIncludeInstallDir = destDir </> winGlutIncludeDir

    winGlutIncSrcs <- getDirectoryContents winExternalGlutIncDir
    copyFilesAction setup winExternalGlutIncDir winGlutIncludeInstallDir
        winGlutIncSrcs
    needContents winGlutIncludeInstallDir

    return . Just $ destDir

-- | These files are needed when building the installer
copyInstExtras :: Rules ()
copyInstExtras = do
    let setup = makeDirectory $ installerPartsDir </> "icons"
    copyFilesRules setup winExtrasSrc installerPartsDir winInstExtrasFiles

pkgrootConfFixup :: OS -> FilePath -> Action ()
pkgrootConfFixup os confFile = do
        putLoud $ "pkgrootConfFixup: " ++ show confFile
        let  OS{..} = os

        confStr <- liftIO . B.readFile $ confFile

        -- Fix up the pkgroot.
        -- Using brute force, replace the first part of the
        -- paths in the conf file with ${pkgroot} so that the conf
        -- is portable.  n.b., not all packages have conf files.
        -- Ref: https://ghc.haskell.org/trac/ghc/ticket/3268
        --   implement the Cabal ${pkgroot} spec extension
        let pkgRoot = T.pack $ toCabalPrefix "lib"

        (return . T.replace pkgRoot "${pkgroot}" . E.decodeUtf8) confStr
            >>= liftIO . B.writeFile confFile . E.encodeUtf8

-- | Take a list of file names, a source dir where they are, a dest dir
-- for where to copy them, a list of additional needs (possibly empty),
-- and set up the needs and rules.
copyFilesRules :: Action () -> FilePath -> FilePath -> [FilePath] -> Rules ()
copyFilesRules setup srcDir dstDir =
    mapM_ (\f -> dstDir </> f %> \_ -> copyFileAction setup srcDir dstDir f)
