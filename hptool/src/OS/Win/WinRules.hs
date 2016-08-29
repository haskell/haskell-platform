{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module OS.Win.WinRules
    ( copyWinTargetExtras
    , pkgrootConfFixup
    , winGhcInstall
    , winRules
    )
  where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Development.Shake
import Development.Shake.FilePath ( (</>), takeDirectory, takeFileName )

import Config
import Dirs
import OS.Internal
import OS.Win.WinNsis
import OS.Win.WinPaths
import OS.Win.WinUtils
import Paths
import Types
import Utils


winRules :: Rules ()
winRules = do
    genNsisData
    genNsisFile
    copyInstExtras

winGhcInstall :: FilePath -> GhcInstallAction
winGhcInstall destDir bc distDir = do
    let untarDir = takeDirectory distDir

    -- (will this cause some race conditions, removing vs populating?)
    command_ [] "mv" [untarDir </> show (bcGhcVersion bc), destDir ]

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


copyWinTargetExtras :: BuildConfig -> Action ()
copyWinTargetExtras bc = do
    -- copy icons
    let mkIconsDir = makeDirectory $ winTargetDir </> "icons"
    copyFilesAction mkIconsDir winExtrasSrc winTargetDir winIconsFiles

    -- copy user's guide docs: ps, pdf, html, etc....
    copyDirAction winExternalDocs winDocTargetDir

    -- copy winghci pieces
    copyDirAction winExternalWinGhciDir winWinGhciTargetDir

    -- copy msys(msys2) pieces
    copyDirAction (winExternalMSysDir bc) winMSysTargetDir

    -- copy cabal executable
    cabalFile <- askCabalExe
    copyFileAction (return ()) (takeDirectory cabalFile) (winHpTargetDir </> "bin") (takeFileName cabalFile)

    -- copy stack executable
    stackFile <- askStackExe
    copyFileAction (return ()) (takeDirectory stackFile) (winHpTargetDir </> "bin") (takeFileName stackFile)



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

copyFileAction :: Action () -> FilePath -> FilePath -> FilePath -> Action ()
copyFileAction setup srcDir dstDir file = do
    need [srcDir </> file]
    setup
    command_ [] "cp" ["-p", srcDir </> file, dstDir </> file]

copyFilesAction :: Action () -> FilePath -> FilePath -> [FilePath] -> Action ()
copyFilesAction setup srcDir dstDir files = do
    setup
    mapM_ (copyFileAction (return ()) srcDir dstDir) files

copyDirAction :: FilePath -> FilePath -> Action ()
copyDirAction srcDir dstDir = do
    needContents srcDir
    makeDirectory dstDir
    -- Two problems: seems that (</>) strips the "." out, so use (++);
    -- second problem is that using an "*" in the path results in an error,
    -- so "/." works better.
    command_ [] "cp" ["-pR", srcDir ++ "/.", dstDir]
    needContents dstDir
