{-# LANGUAGE RecordWildCards #-}

module OS.Mac
    ( macOsFromConfig
    )
  where

import Control.Monad (forM_)
import Data.Version
import Development.Shake
import Development.Shake.FilePath

import Dirs
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

    -- Both HP and GHC are stored under prefixes that are based on the version
    -- and architecture of the GHC compiler. The linkage between the two is
    -- fixed, and making it explicit this way simplifies installation and
    -- removal. A consequence of this choice is that you cannot have two
    -- versions of the platform installed that use the same compiler and arch.
    -- This is reasonable, as minor revs of the platform will generally use the
    -- same compiler, and should overwrite older revs of the same version, and
    -- major revs of the platform have always used a newer compiler. Finally,
    -- most people refer to the version of Haskell they have by the version of
    -- the compiler.
    osHpPrefix = "/Library/Haskell"
                        </> "ghc-" ++ versionAndArch ghcVersion
    osGhcPrefix = "/Library/Frameworks/GHC.framework/Versions"
                        </> versionAndArch ghcVersion </> "usr"

    hpTargetDir = targetDir </+> osHpPrefix

    osPackageTargetDir p = osHpPrefix </> "lib" </> packagePattern p
    osPackageInstallAction p = do
        let confFile = packageTargetConf p
        let regDir = hpTargetDir </> "lib" </> "registrations"
        let regFile = regDir </> show p
        makeDirectory regDir
        hasReg <- doesFileExist confFile
        if hasReg
            then command_ [] "cp" [confFile, regFile]
            else command_ [] "rm" ["-f", regFile]

    hpBinDir = hpTargetDir </> "bin"
    hpDocDir = hpTargetDir </> "doc"
    versionFile = hpTargetDir </> "version-" ++ showVersion hpVersion
    cabalFile = hpTargetDir </> "haskell-platform.cabal"

    osTargetAction = do
        need [dir hpBinDir, versionFile, cabalFile ]

    osDocAction = do
        docFiles <- getDirectoryFiles "" [osxExtras </> "doc/*"]
        forM_ docFiles $ \f -> do
            copyFile' f $ hpDocDir </> takeFileName f

    productName =
        "Haskell Platform " ++ showVersion hpVersion ++ archBits bcArch

    osProduct = productDir </> productName <.> "pkg"
    signedProduct = productDir </> (productName ++ "-signed") <.> "pkg"

    hpPkgFile = installerPartsDir </> "HaskellPlatform.pkg"
    ghcPkgFile = installerPartsDir </> "GHC.pkg"

    archBits "i386"     = " 32bit"
    archBits "x86_64"   = " 64bit"
    archBits a          = ' ' : a

    osxExtras = "hptool/os-extras/osx"
    osxInstallScripts   = osxExtras </> "installer-scripts"
    osxInstallResources = osxExtras </> "installer-resources"
    osxInstallerDist    = osxExtras </> "installer.dist"

    osRules _hpRelease _bc = do
        hpBinDir */> \d -> do
            makeDirectory d
            binFiles <- getDirectoryFiles "" [osxExtras </> "bin/*"]
            forM_ binFiles $ \f -> do
                if takeExtension f == ".hs"
                    then compileToBin f $ d </> takeBaseName f
                    else copyFile'    f $ d </> takeFileName f

        versionFile *> \out -> do
            writeFileChanged out $ unlines
                [ "platform " ++ showVersion hpVersion
                , "ghc      " ++ showVersion ghcVersion
                , "arch     " ++ bcArch
                ]

        cabalFile *> copyFile' hpCabalFile

        ghcPkgFile *> \out -> do
            need [vdir ghcVirtualTarget]
            command_ [] "pkgbuild"
                [ "--identifier", "org.haskell.HaskellPlatform.GHC"
                                  ++ ghcPkgMajorVer ++ ".pkg"
                , "--version", ghcPkgMinorVer
                , "--install-location", "/Library/Frameworks"
                , "--root", "build/target/Library/Frameworks"
                , out
                ]

        hpPkgFile *> \out -> do
            need [targetDir]  -- FIXME(mzero): could be more specific
            needContents osxInstallScripts
            command_ []
                "pkgbuild"
                [ "--identifier", "org.haskell.HaskellPlatform.Libraries."
                                  ++ hpPkgMajorVer ++ ".pkg"
                , "--version", hpPkgMinorVer
                , "--install-location", "/Library/Haskell"
                , "--root", "build/target/Library/Haskell"
                , "--scripts", osxInstallScripts
                , out
                ]

        osProduct *> \out -> do
            need [ghcPkgFile, hpPkgFile, osxInstallerDist]
            needContents osxInstallResources
            command_ []
                "productbuild"
                [ "--identifier", "org.haskell.HaskellPlatform."
                                  ++ hpPkgMajorVer ++ ".pkg"
                , "--version", hpPkgMinorVer
                , "--resources", osxInstallResources
                , "--distribution", osxInstallerDist
                , "--package-path", installerPartsDir
                , out
                ]
            -- shell quoting
            let q s = "'" ++ s ++ "'"
            putNormal $ "When ready to distribute, sign & check with:"
            putNormal $ "    productsign --sign 'Developer ID Installer:' "
                             ++ q osProduct ++ " " ++ q signedProduct
            putNormal $ "    spctl -a -v --type install " ++ q signedProduct

    (ghcPkgMajorVer, ghcPkgMinorVer) =
        let (maj,(m0:_)) = splitAt 3 $ versionBranch ghcVersion ++ repeat 0 in
        (concatMap show maj, show $ m0 + 1)

    (hpPkgMajorVer, hpPkgMinorVer) =
        let (maj,(m1:m0:_)) = splitAt 2 $ versionBranch hpVersion ++ repeat 0 in
        (concatMap show maj, show $ 10*m1 + m0 + 1)


compileToBin :: FilePath -> FilePath -> Action ()
compileToBin src dst = do
    need [src]
    makeDirectory outputDir
    command_ [] "ghc"
        [ "-o", dst <.> exe
        , "-O2"
        , "-Wall"
        , "-outputdir", outputDir
        , src
        ]
        -- this is not built with the dist GHC because localCommand'
        -- requires GhcDist, which is a circular dependency
    command_ [] "strip" [dst]
  where
    outputDir = installerPartsDir </> "build"
