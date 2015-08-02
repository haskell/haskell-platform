{-# LANGUAGE Rank2Types, RecordWildCards #-}

module OS.Posix
    ( posixOS
    )
  where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Development.Shake
import Development.Shake.FilePath

import Dirs
import OS.Internal
import Paths
import Templates
import Types
import Utils

posixOS :: BuildConfig -> OS
posixOS BuildConfig{..} = OS{..}
  where
    HpVersion{..} = bcHpVersion
    GhcVersion{..} = bcGhcVersion

    prefix = fromMaybe "/usr/local/haskell" bcPrefix
    absPrefix = "/" </> prefix
    relPrefix = drop 1 absPrefix

    versionName = "ghc-" ++ showVersion ghcVersion ++ "-" ++ bcArch
    absVersionDir = absPrefix </> versionName
    relVersionDir = relPrefix </> versionName

    osHpPrefix = absVersionDir
    osGhcPrefix = absVersionDir

    osGhcLocalInstall = GhcInstallConfigure
    osGhcTargetInstall = GhcInstallConfigure

    osPackageTargetDir p = osHpPrefix </> "lib" </> packagePattern p
    osDoShared = True
    osPackagePostRegister _ = return ()
    osPackageInstallAction p = do
        let confFile = packageTargetConf p
        let regDir = targetDir </+> osHpPrefix </> "etc" </> "registrations"
        let regFile = regDir </> show p
        makeDirectory regDir
        hasReg <- doesFileExist confFile
        if hasReg
            then command_ [] "cp" [confFile, regFile]
            else command_ [] "rm" ["-f", regFile]

    hpTargetDir = targetDir </+> osHpPrefix
    hpBinDir = hpTargetDir </> "bin"
    versionFile = hpTargetDir </> "version-" ++ showVersion hpVersion
    cabalFile = hpTargetDir </> "haskell-platform.cabal"

    osTargetAction = do
        need [vdir hpBinDir, versionFile, cabalFile ]

    osGhcDbDir = "lib" </> show bcGhcVersion </> "package.conf.d"
    osGhcPkgHtmlFieldExtras = []
    osPlatformPkgPathMunge = flip const
    osGhcPkgPathMunge = flip const
    osPkgHtmlDir pkg = osPackageTargetDir pkg </> "doc" </> "html"
    osDocAction = return ()
    osProduct = productDir </> productName ++ ".tar.gz"

    usrLocalTar = productDir </> "hp-usr-local" ++ ".tar.gz"
    installScript = extrasDir </> "installer" </> "install-haskell-platform.sh"

    productName =
        "haskell-platform-" ++ showVersion hpVersion ++ "-unknown-posix-" ++ bcArch

    genericExtrasSrc = "hptool/os-extras/posix"

    osRules _hpRelease _bc = do
        extrasDir %/> \dst -> do
            ctx <- platformContext
            let ctx' = ctxConcat [ assocListContext [("absVersionDir", absVersionDir)], ctx ]
            copyExpandedDir ctx' genericExtrasSrc dst

        osProduct %> \out -> do
            let installFile = takeFileName installScript
            need [ usrLocalTar, dir extrasDir]
            command_ [] "cp" [ installScript, productDir ]
            command_ [Cwd productDir]
                "tar" ["czf", out ® targetDir, installFile, usrLocalTar ® productDir ]
            mapM_ putNormal
                [ replicate 72 '-'
                , "To install this build:"
                , "1) copy " ++ out ++ " to the target machine"
                , "2) untar it (creates files in the working directory)"
                , "3) as root, run the script ./" ++ installFile
                ]

        usrLocalTar %> \out -> do
            need [phonyTargetDir, vdir ghcVirtualTarget]
            command_ [Cwd targetDir]
                "tar" ["czf", out ® targetDir, hpTargetDir ® targetDir]

        versionFile %> \out -> do
            writeFileChanged out $ unlines
                [ "platform " ++ showVersion hpVersion
                , "ghc      " ++ showVersion ghcVersion
                , "arch     " ++ bcArch
                ]

        cabalFile %> copyFile' hpCabalFile

        hpBinDir ~/> do
            -- These items are being merged into the bin directory, which has
            -- bin items from the packages, hence it is a virtual target dir.
            makeDirectory hpBinDir
            need [dir extrasDir]
            binFiles <- getDirectoryFiles "" [extrasDir </> "bin/*"]
            forM_ binFiles $ \f -> do
                copyFile' f $ hpBinDir </> takeFileName f
            return Nothing

    osPackageConfigureExtraArgs _pkg =
        [ override "prefix"     "/usr/local"                    (absPrefix </> "$compiler-$arch")
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
        --, stock    "sysconfdir" "$prefix/etc"
        ]
      where
        stock    k  v0    = "--" ++ k ++ "=" ++ v0  -- the 1.18 default value
        override k _v0 v1 = "--" ++ k ++ "=" ++ v1  -- our override
        -- N.B.: Because the default cabal layout changed in 1.18, and because the
        -- host cabal is used to build the packages, and it might be pre-1.18, we
        -- need to specify every dir parameter explicitly.
        --
        -- See also the file notes/cabal-layouts
