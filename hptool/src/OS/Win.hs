{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings #-}

module OS.Win
    ( winOsFromConfig
    )
  where

import Control.Monad ( void, when )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
#if !MIN_VERSION_Cabal(1,22,0)
import qualified Distribution.InstalledPackageInfo as C
#endif
import qualified Distribution.Package as C

import Dirs
import LocalCommand
import OS.Internal
import OS.Win.WinPaths
import OS.Win.WinRules
import OS.Win.WinUtils
import Paths
import Types
import Utils


winOsFromConfig :: BuildConfig -> OS
winOsFromConfig BuildConfig{..} = os
  where
    os = OS{..}

    HpVersion{..} = bcHpVersion
    GhcVersion{..} = bcGhcVersion

    osHpPrefix  = winHpPrefix
    osGhcPrefix = winGhcPrefix

    osGhcLocalInstall =
        GhcInstallCustom $ winGhcInstall ghcLocalDir
    osGhcTargetInstall =
        -- Windows installs HP and GHC in a single directory, so creating
        -- dependencies on the contents of winGhcTargetDir won't account
        -- for the HP pieces.  Also, for Windows, the ghc-bindist/local and
        -- the GHC installed into the targetDir should be identical.
        -- osTargetAction is the right place to do the targetDir snapshot.
        GhcInstallCustom $ \bc distDir -> do
            void $ winGhcInstall winGhcTargetDir bc distDir
            return ghcLocalDir

    osPackageTargetDir p = winHpPrefix </> packagePattern p

    -- The ghc builds for Windows do not have pre-built .dyn_hi files
    -- (Revisit this in future versions)
    osDoShared = False

    osPackagePostRegister p = do
        let confFile = packageTargetConf p
        whenM (doesFileExist confFile) $ pkgrootConfFixup os confFile

    osPackageInstallAction p = do
        putLoud $ "osPackageInstallAction: " ++ show p

        -- "install" the packages into winTargetDir.
        --
        -- This is not "installing"; this is simply "copying"; later on, we
        -- check consistency to be sure.  Furthermore, the Shake actions
        -- are run in parallel, so registering via ghc-pkg at this point
        -- can result in failures, as the conf files need to be installed
        -- in dependency order, which cannot be expected due
        -- to the parallel builds coupled with laziness in Shake actions.
        -- These could possibly be resolved by creating a Rule for the
        -- ghc-pkg register, but this might hurt the parallel builds.
        let confFile = packageTargetConf p
        whenM (doesFileExist confFile) $ do
            confStr <- liftIO . B.readFile $ confFile
            pkgInfo <- parseConfFile confFile (B8.unpack confStr)
            let (C.InstalledPackageId pkgid) = C.installedPackageId pkgInfo
                -- need the long name of the package
                pkgDbConf = winGhcTargetPackageDbDir </> pkgid <.> "conf"
            command_ [] "cp" ["-p", confFile, pkgDbConf]

    whenM :: (Monad m) => m Bool -> m () -> m ()
    whenM mp m = mp >>= \p -> when p m

    osTargetAction = do
        -- Now, targetDir is actually ready to snapshot (we skipped doing
        -- this in osGhcTargetInstall).
        void $ getDirectoryFiles "" [targetDir ++ "//*"]

    osGhcDbDir = winGhcPackageDbDir

    osGhcPkgHtmlFieldExtras = ["--no-expand-pkgroot"]

    osPlatformPkgPathMunge base (HaddockPkgLoc p i) =
        HaddockPkgLoc (relativeToDir (expandPkgroot p) base) i
    osGhcPkgPathMunge base (HaddockPkgLoc p i) =
        HaddockPkgLoc (relativeToDir (expandTopdir p) base) i

    osPkgHtmlDir pkgid =
        expandPkgroot . expandPrefix . expandPkgid . expandDocdir $ htmldir
      where
        expandPrefix = replace "$prefix" (targetDir </> osHpPrefix)
        expandPkgid = replace "$pkgid" (show pkgid)
        expandDocdir = replace "$docdir" docdir


    -- On Windows, ghc uses $topdir; and when installed, this is
    -- <installdir>/lib
    expandTopdir = replace "$topdir" (targetDir </> "lib")

    -- On Windows, HP conf files use ${pkgroot}; and when installed this is
    -- <installdir>/lib
    expandPkgroot = replace "${pkgroot}" (targetDir </> "lib")

    replace srchT replc = T.unpack . T.replace srchT (T.pack replc) . T.pack


    osDocAction = return ()

    osProduct = winProductFile bcIncludeExtra hpVersion bcArch

    osRules _rel bc = do
        winRules

        osProduct %> \_ -> do
            need $ [dir ghcLocalDir, targetDir, vdir ghcVirtualTarget]
                   ++ winNeeds

            copyWinTargetExtras bc

            -- Now, it is time to make sure there are no problems with the
            -- conf files copied to
            localCommand' [] "ghc-pkg"
                [ "recache"
                , "--package-db=" ++ winGhcTargetPackageDbDir ]
            localCommand' [] "ghc-pkg"
                [ "check"
                , "--package-db=" ++ winGhcTargetPackageDbDir ]

          -- Build installer now; makensis must be run in installerPartsDir
            command_ [Cwd installerPartsDir] "makensis" [nsisFileName]

    osPackageConfigureExtraArgs _ =
        [ "--prefix=" ++ toCabalPrefix osHpPrefix
        , "--libsubdir=$pkgid"
        , "--datasubdir=$pkgid"
        , "--docdir=" ++ docdir
        ]
    htmldir = "$docdir/html"
    docdir = "$prefix/doc/$pkgid"
