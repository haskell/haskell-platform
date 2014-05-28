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

    osProduct = productDir
        </> ("Haskell Platform " ++ showVersion hpVersion ++ archBits bcArch)
        <.> "pkg"

    hpPkgFile = installerPartsDir </> "HaskellPlatform.pkg"
    ghcPkgFile = installerPartsDir </> "GHC.pkg"

    archBits "i386"     = " 32bit"
    archBits "x86_64"   = " 64bit"
    archBits a          = ' ' : a

    osProductAction = do
        makeDirectory installerPartsDir
        command_ []
            "pkgbuild"
            [ "--identifier", "org.haskell.HaskellPlatform.GHC"
                              ++ ghcPkgMajorVer ++ ".pkg"
            , "--version", ghcPkgMinorVer
            , "--install-location", "/Library/Frameworks"
            , "--root", "build/target/Library/Frameworks"
            , hpPkgFile
            ]
        command_ []
            "pkgbuild"
            [ "--identifier", "org.haskell.HaskellPlatform.Libraries."
                              ++ hpPkgMajorVer ++ ".pkg"
            , "--version", hpPkgMinorVer
            , "--install-location", "/Library/Haskell"
            , "--root", "build/target/Library/Haskell"
            , "--scripts", "hptool/os-extras/osx/installer-scripts"
            , ghcPkgFile
            ]
        command_ []
            "productbuild"
            [ "--identifier", "org.haskell.HaskellPlatform."
                              ++ hpPkgMajorVer ++ ".pkg"
            , "--version", hpPkgMinorVer
            , "--resources", "hptool/os-extras/osx/installer-resources"
            , "--distribution", "hptool/os-extras/osx/installer.dist"
            , "--package-path", installerPartsDir
            , osProduct
            ]

    (ghcPkgMajorVer, ghcPkgMinorVer) =
        let (maj,(m0:_)) = splitAt 3 $ versionBranch ghcVersion ++ repeat 0 in
        (concatMap show maj, show $ m0 + 1)

    (hpPkgMajorVer, hpPkgMinorVer) =
        let (maj,(m1:m0:_)) = splitAt 2 $ versionBranch hpVersion ++ repeat 0 in
        (concatMap show maj, show $ 10*m1 + m0 + 1)

{--
osPolishAction = do
    binFiles <- getDirectoryFiles "" ["data/mac/bin/*"]
    forM_ binFiles $ \f -> do
        if takeExtension f == ".hs"
            then compileToBin f
            else copyFile' f bin

    lhSymLink hpVersion "current"
    lhSymLink "current/bin" "bin"
    lhSymlink "current/lib" "lib"
  where
    lhSymLink to from = do
        bracket
            (getWorkingDirectory <* changeWorkingDirectory lhDir)
            changeWorkingDirectory
            \_ -> createSymbolicLink to from

    lhDir = targetDir </+> osHpPrefix




pkgbuild --identifier org.haskell.HaskellPlatform.GHC.782.pkg
    --version 1
    --install-location "/Library/Frameworks"
    --root build/target/Library/Frameworks
    build/product/GHC.pkg
pkgbuild --identifier org.haskell.HaskellPlatform.Libraries.20141.pkg
    --version 1
    --install-location "/Library/Haskell"
    --root build/target/Library/Haskell
    build/product/HaskellPlatform.pkg
productbuild --resources hptool/os-extras/osx/installer-resources
    --identifier org.haskell.HaskellPlatform.20141
    --version 1
    --distribution hptool/os-extras/osx/installer.dist
    --package-path build/product
    build/product/HP-Installer.pkg


--}
