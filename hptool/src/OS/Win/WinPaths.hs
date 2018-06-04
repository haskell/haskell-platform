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
-- (this is everything (including GHC) that is not MSys, extralibs, or ghc doc)
nsisFileName :: FilePath
nsisFileName = "Nsisfile.nsi"

-- | Parts needed to build the installer: the nsis file, and the lists of files
nsisFile :: FilePath
nsisFile = installerPartsDir </> nsisFileName

-- | The full sub-installer file name; it is internal only;
--   launched by bootstrapper.
nsisFileProductFileName :: FilePath
nsisFileProductFileName = "HP-setup" <.> "exe"

-- | Directory where the MSys sub-installer file is built.
nsisFileProductFile :: FilePath
nsisFileProductFile = productDir </> nsisFileProductFileName

commonNshFileName :: FilePath
commonNshFileName = "CommonHP.nsh"

commonNshFile :: FilePath
commonNshFile = installerPartsDir </> commonNshFileName

-- | MSys installation data files
msysNsisFileName :: FilePath
msysNsisFileName = "MSys.nsi"

msysNsisFile :: FilePath
msysNsisFile = installerPartsDir </> msysNsisFileName

msysNsisInstDat :: FilePath
msysNsisInstDat = installerPartsDir </> "MSys_inst.dat"

msysNsisUninstDat :: FilePath
msysNsisUninstDat = installerPartsDir </> "MSys_uninst.dat"

-- | The msys sub-installer file name; it is internal only
msysProductFileName :: FilePath
msysProductFileName = "MSys-setup" <.> "exe"

-- | Directory where the MSys sub-installer file is built.
msysProductFile :: FilePath
msysProductFile = productDir </> msysProductFileName


-- | Extralibs installation data files
extralibsNsisFileName :: FilePath
extralibsNsisFileName = "Extralibs.nsi"

extralibsNsisFile :: FilePath
extralibsNsisFile = installerPartsDir </> extralibsNsisFileName

extralibsNsisInstDat :: FilePath
extralibsNsisInstDat = installerPartsDir </> "Extralibs_inst.dat"

extralibsNsisUninstDat :: FilePath
extralibsNsisUninstDat = installerPartsDir </> "Extralibs_uninst.dat"

-- | The extralibs sub-installer file name; it is internal only
extralibsProductFileName :: FilePath
extralibsProductFileName = "Extralibs-setup" <.> "exe"

-- | Directory where the extralibs sub-installer file is built.
extralibsProductFile :: FilePath
extralibsProductFile = productDir </> extralibsProductFileName


-- | GHC installation data files
ghcNsisFileName :: FilePath
ghcNsisFileName = "GHC.nsi"

ghcNsisFile :: FilePath
ghcNsisFile = installerPartsDir </> ghcNsisFileName

ghcNsisInstDat :: FilePath
ghcNsisInstDat = installerPartsDir </> "GHC_inst.dat"

ghcNsisUninstDat :: FilePath
ghcNsisUninstDat = installerPartsDir </> "GHC_uninst.dat"

-- | The GHC sub-installer file name; it is internal only
ghcProductFileName :: FilePath
ghcProductFileName = "GHC-setup" <.> "exe"

-- | Directory where the GHC sub-installer file is built.
ghcProductFile :: FilePath
ghcProductFile = productDir </> ghcProductFileName


-- | GHC doc installation data files
ghcDocNsisFileName :: FilePath
ghcDocNsisFileName = "GHCDoc.nsi"

ghcDocNsisFile :: FilePath
ghcDocNsisFile = installerPartsDir </> ghcDocNsisFileName

ghcDocNsisInstDat :: FilePath
ghcDocNsisInstDat = installerPartsDir </> "GHCDoc_inst.dat"

ghcDocNsisUninstDat :: FilePath
ghcDocNsisUninstDat = installerPartsDir </> "GHCDoc_uninst.dat"

-- | The GHC Doc sub-installer file name; it is internal only
ghcDocProductFileName :: FilePath
ghcDocProductFileName = "GHCDoc-setup" <.> "exe"

-- | Directory where the GHC doc sub-installer file is built.
ghcDocProductFile :: FilePath
ghcDocProductFile = productDir </> ghcDocProductFileName


-- | A tiny installer just to launch the full installer and also to
--   modify/install the user's cabal config, which is kept separate
--   so that it does not elevate to admin and updates the config as the user.
--   Since this is the overall wrapper, it gets the "main" file name.
bootstrapNsisFileName :: FilePath
bootstrapNsisFileName = "Bootstrapper.nsi"

bootstrapNsisFile :: FilePath
bootstrapNsisFile = installerPartsDir </> bootstrapNsisFileName


-- | Pre-built or unchanging files that need to be included in the installer
winInstExtras :: [FilePath]
winInstExtras = map (installerPartsDir </>) winInstExtrasFiles


-- | Templates to generate some of the installer data files
nsiTemplate :: FilePath
nsiTemplate = winTemplates </> "Nsisfile.nsi.mu"

subInstNsiTemplate :: FilePath
subInstNsiTemplate = winTemplates </> "SubInstall.nsi.mu"

commonNshTemplate :: FilePath
commonNshTemplate = winTemplates </> "CommonHP.nsh.mu"

bootstrapNsiTemplate :: FilePath
bootstrapNsiTemplate = winTemplates </> "Bootstrapper.nsi.mu"

nsisInstDatTmpl :: FilePath
nsisInstDatTmpl = winTemplates </> "inst.dat.mu"

nsisUninstDatTmpl :: FilePath
nsisUninstDatTmpl = winTemplates </> "uninst.dat.mu"


-- | Template file and end-result file for the overall index.html
docIndexFile :: FilePath
docIndexFile = winDocTargetDir </> "index.html"

docIndexTmpl :: FilePath
docIndexTmpl = winTemplates </> "index.html.mu"


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

-- | This is the place for pre-built files (e.g., Glut libraries), which
-- will be installed, but are not part of the build or of hptool.
winExternalSrc :: FilePath
winExternalSrc = "winExternalSrc"

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
  where versionAndArch = showVersion hpv ++ (if isFull then "-full" else "-core") ++ '-' : arch

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
winNeeds = [ nsisFile
           , docIndexFile
           ]
           ++ winInstExtras

winSubProductFiles :: [FilePath]
winSubProductFiles = [
    ghcDocProductFile, ghcProductFile, extralibsProductFile, msysProductFile ]
