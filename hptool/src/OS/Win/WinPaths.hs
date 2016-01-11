module OS.Win.WinPaths where

import Development.Shake.FilePath ( (</>), (<.>), exe )
import Data.Version (showVersion, Version)

import Paths ( installerPartsDir, productDir, targetDir )
import Types


-- | The sub-directory under "Program Files" where the platform goes
winPFHaskellPlatformDir :: FilePath
winPFHaskellPlatformDir = "Haskell Platform"

-- | Some additional data files and templates needed to build the installer
winExtrasSrc :: FilePath
winExtrasSrc = "hptool/os-extras/win"

winTemplates :: FilePath
winTemplates = winExtrasSrc </> "templates"

-- | File name of the data file used by NSIS to create the installer exe
nsisFileName :: FilePath
nsisFileName = "Nsisfile.nsi"

-- | Parts needed to build the installer: the nsis file, and the lists of files
nsisFile :: FilePath
nsisFile = installerPartsDir </> nsisFileName

nsisInstDat :: FilePath
nsisInstDat = installerPartsDir </> "inst.dat"

nsisUninstDat :: FilePath
nsisUninstDat = installerPartsDir </> "uninst.dat"

-- | Pre-built or unchanging files that need to be included in the installer
winInstExtras :: [FilePath]
winInstExtras = map (installerPartsDir </>) winInstExtrasFiles

-- | Templates to generate some of the installer data files
nsiTemplate :: FilePath
nsiTemplate = winTemplates </> "Nsisfile.nsi.mu"

nsisInstDatTmpl :: FilePath
nsisInstDatTmpl = winTemplates </> "inst.dat.mu"

nsisUninstDatTmpl :: FilePath
nsisUninstDatTmpl = winTemplates </> "uninst.dat.mu"

-- | Some scripts and unchanging data files needed for the installer
winInstExtrasFiles :: [FilePath]
winInstExtrasFiles = [ "EnvVarUpdate.nsh"
                     , "CreateInternetShortcut.nsh"
                     , "LICENSE"
                     , "welcome.bmp"
                     ] ++ winIconsFiles

winIconsTargetFiles :: [FilePath]
winIconsTargetFiles = map (winTargetDir </>) winIconsFiles

winIconsFiles :: [FilePath]
winIconsFiles = [ "icons/installer.ico"
                , "icons/hsicon.ico"
                , "icons/hackage.ico"
                ]

-- | This is the place for pre-built files (e.g., docs from GHC), which
-- will be installed, but are not part of the build or of hptool.
winExternalSrc :: FilePath
winExternalSrc = "winExternalSrc"

-- | These will be copied to the top-level doc directory
winExternalDocs :: FilePath
winExternalDocs = winExternalSrc </> "doc"

-- | Source info (paths & names) for GLUT pieces, which will be simply copied
winExternalGlut :: FilePath
winExternalGlut = winExternalSrc </> "glut"

-- | The GLUT library and DLL are architecture-specific
winExternalGlutLibDir :: BuildConfig -> FilePath
winExternalGlutLibDir bc = winExternalGlut </> "lib" </> bcArch bc

winExternalGlutIncDir :: FilePath
winExternalGlutIncDir = winExternalGlut </> "include"

-- | Glut pieces and their generic destinations
winGlutDllFile, winGlutLibFile, winGlutIncludeDir, winGlutLibDir,
    winGlutDllDir :: FilePath

-- | GLUT DLL (renamed to glut32.dll even if 64-bit platform)
winGlutDllFile    = "glut32.dll"
-- | GLUT library (renamed to libglut32.a even if 64-bit platform)
winGlutLibFile    = "libglut32.a"
-- | Where the GLUT include files go
winGlutIncludeDir = "mingw/include/GL"
-- | Where the GLUT library goes
winGlutLibDir     = "mingw/lib"
-- | Where the GLUT DLL goes
winGlutDllDir     = "bin"

-- | winghci pieces; the contents of this dir will be copied to install
winExternalWinGhciDir :: FilePath
winExternalWinGhciDir = winExternalSrc </> "winghci"

-- | Install dir for the winghci tool
winWinGhciTargetDir :: FilePath
winWinGhciTargetDir = winTargetDir </> "winghci"

winExternalMSysDir :: BuildConfig -> FilePath
winExternalMSysDir bc = winExternalSrc </> "msys" </> bcArch bc

winMSysTargetDir :: FilePath
winMSysTargetDir = winTargetDir </> "msys"

-- | ghc.exe file, relative to the install
winGhcExeBin :: FilePath
winGhcExeBin = "bin" </> "ghc" <.> exe

-- | The relative-directory representing the install location for the product.
winInstPrefix :: FilePath
winInstPrefix = ""

-- | The GHC pieces go in the same place as the product.
winGhcPrefix :: FilePath
winGhcPrefix = ""

-- | Where HP-specific packages go, under the installed tree
winHpPrefix :: FilePath
winHpPrefix = "lib/extralibs"

-- | The installation, as staged-out, into the targetDir
winTargetDir :: FilePath
winTargetDir = targetDir </> winInstPrefix

-- | When staging the GHC pieces, they go here
winGhcTargetDir :: FilePath
winGhcTargetDir = winTargetDir </> winGhcPrefix

-- | The targetDir is the place in the build tree where we stage the install.
winHpTargetDir :: FilePath
winHpTargetDir = winTargetDir </> winHpPrefix

-- | Top-level docs directory
winDocTargetDir :: FilePath
winDocTargetDir = winTargetDir </> "doc"

-- | The installer file name, dependent on the HP version and architecture
winProductFileName :: Bool -> Version -> String -> FilePath
winProductFileName isFull hpv arch =
    ("HaskellPlatform-" ++ versionAndArch ++ "-setup") <.> "exe"
  where versionAndArch = showVersion hpv ++ (if isFull then "-full" else "-minimal") ++ '-' : arch

-- | Directory where the installer file is built.
winProductFile :: Bool -> Version -> String -> FilePath
winProductFile isFull hpv arch = productDir </> winProductFileName isFull hpv arch

-- | Relative to the install dir
winGhcPackageDbDir :: FilePath
winGhcPackageDbDir = "lib/package.conf.d"

-- | Relative the top-level build dir
winGhcTargetPackageDbDir :: FilePath
winGhcTargetPackageDbDir = winTargetDir </> winGhcPackageDbDir

-- | Additional build targets needed for the Windows version of HP
winNeeds :: [FilePath]
winNeeds = [ nsisFile, nsisInstDat, nsisUninstDat ]
           ++ winInstExtras
