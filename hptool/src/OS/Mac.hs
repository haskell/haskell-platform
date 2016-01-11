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
import Templates
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

    osGhcLocalInstall = GhcInstallConfigure
    osGhcTargetInstall = GhcInstallConfigure

    osPackageTargetDir p = osHpPrefix </> "lib" </> packagePattern p
    osDoShared = True
    osPackagePostRegister _ = return ()
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
        need [vdir hpBinDir, versionFile, cabalFile ]

    osGhcDbDir = "lib" </> show bcGhcVersion </> "package.conf.d"

    osGhcPkgHtmlFieldExtras = []
    osPlatformPkgPathMunge = flip const
    osGhcPkgPathMunge = flip const
    osPkgHtmlDir pkg = osPackageTargetDir pkg </> "doc" </> "html"

    osDocAction = do
        need [dir extrasDir]
        docFiles <- getDirectoryFiles "" [extrasDir </> "doc/*"]
        forM_ docFiles $ \f -> do
            copyFile' f $ hpDocDir </> takeFileName f

    productName =
        "Haskell Platform " ++ showVersion hpVersion ++ (if bcIncludeExtra then " Full" else " Minimal") ++ archBits bcArch

    osProduct = productDir </> productName <.> "pkg"
    signedProduct = productDir </> (productName ++ "-signed") <.> "pkg"

    hpPkgFile = installerPartsDir </> "HaskellPlatform.pkg"
    ghcPkgFile = installerPartsDir </> "GHC.pkg"

    archBits "i386"     = " 32bit"
    archBits "x86_64"   = " 64bit"
    archBits a          = ' ' : a

    osxExtrasSrc = "hptool/os-extras/osx"
        -- these will be template expanded (if needed) into extrasDir

    osxInstallScripts   = extrasDir </> "installer-scripts"
    osxInstallResources = extrasDir </> "installer-resources"
    osxInstallerDist    = extrasDir </> "installer.dist"

    osRules _hpRelease _bc = do
        extrasDir %/> \dst -> do
            ctx <- platformContext
            copyExpandedDir ctx osxExtrasSrc dst

        hpBinDir ~/> do
            -- These items are being merged into the bin directory, which has
            -- bin items from the packages, hence it is a virtual target dir.
            makeDirectory hpBinDir
            need [dir extrasDir]
            binFiles <- getDirectoryFiles "" [extrasDir </> "bin/*"]
            forM_ binFiles $ \f -> do
                if takeExtension f == ".hs"
                    then compileToBin f $ hpBinDir </> takeBaseName f
                    else copyFile'    f $ hpBinDir </> takeFileName f
            return Nothing

        versionFile %> \out -> do
            writeFileChanged out $ unlines
                [ "platform " ++ showVersion hpVersion
                , "ghc      " ++ showVersion ghcVersion
                , "arch     " ++ bcArch
                ]

        cabalFile %> copyFile' hpCabalFile

        ghcPkgFile %> \out -> do
            need [vdir ghcVirtualTarget]
            command_ [] "pkgbuild"
                [ "--identifier", "org.haskell.HaskellPlatform.GHC"
                                  ++ ghcPkgMajorVer ++ ".pkg"
                , "--version", ghcPkgMinorVer
                , "--install-location", "/Library/Frameworks"
                , "--root", "build/target/Library/Frameworks"
                , out
                ]

        hpPkgFile %> \out -> do
            need [targetDir, dir extrasDir]  -- FIXME(mzero): could be more specific
            command_ []
                "pkgbuild"
                [ "--identifier", "org.haskell.HaskellPlatform.Libraries."
                                  ++ hpPkgMajorVer ++ (if bcIncludeExtra then "-full" else "-minimal") ++ ".pkg"
                , "--version", hpPkgMinorVer
                , "--install-location", "/Library/Haskell"
                , "--root", "build/target/Library/Haskell"
                , "--scripts", osxInstallScripts
                , out
                ]

        osProduct %> \out -> do
            need [ghcPkgFile, hpPkgFile, dir extrasDir]
            command_ []
                "productbuild"
                [ "--identifier", "org.haskell.HaskellPlatform."
                                  ++ hpPkgMajorVer ++ (if bcIncludeExtra then "-full" else "-minimal") ++ ".pkg"
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

    osPackageConfigureExtraArgs pkg =
        [ override "prefix"     "/usr/local"                    "/Library/Haskell/$compiler-$arch"
        , stock    "bindir"     "$prefix/bin"
        , stock    "libdir"     "$prefix/lib"
        , override "libsubdir"  "$arch-$os-$compiler/$pkgid"    "$pkgid"
        , stock    "libexecdir" "$prefix/libexec"
        , stock    "datadir"    "$prefix/share"
        , override "datasubdir" "$arch-$os-$compiler/$pkgid"    "$pkgid"
        , override "docdir"     "$datadir/doc/$arch-$os-$compiler/$pkgid"
                                                                "$prefix/lib/$pkgid/doc"
        , stock    "htmldir"    "$docdir/html"
        , stock    "haddockdir" "$htmldir"
        , stock    "sysconfdir" "$prefix/etc"
        ]
        ++ perPackageExtraArgs
      where
        stock    k  v0    = "--" ++ k ++ "=" ++ v0  -- the 1.18 default value
        override k _v0 v1 = "--" ++ k ++ "=" ++ v1  -- our override
        -- N.B.: Because the default cabal layout changed in 1.18, and because the
        -- host cabal is used to build the packages, and it might be pre-1.18, we
        -- need to specify every dir parameter explicitly.
        --
        -- See also the file notes/cabal-layouts
        perPackageExtraArgs = case pkgName pkg of
            "GLUT" -> [ "--ghc-options=-optl-Wl,-framework,GLUT" ]
                        -- see https://ghc.haskell.org/trac/ghc/ticket/10568
            _ -> []

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
