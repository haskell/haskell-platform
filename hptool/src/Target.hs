{-# LANGUAGE RecordWildCards #-}

module Target
    ( targetRules
    )
  where

import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import Development.Shake
import Development.Shake.FilePath

import Config
import Dirs
import LocalCommand
import HaddockMaster
import OS
import Paths
import PlatformDB
import Types
import Utils
import Control.Exception(evaluate)
import Data.List
import Data.Char


targetRules :: BuildConfig -> Rules ()
targetRules bc = do
    buildRules
    installRules bc
    phonyTargetDir ~> do
        hpRel <- askHpRelease
        bc' <- askBuildConfig
        let OS{..} = osFromConfig bc'
        let packages = platformPackages (bcIncludeExtra bc') hpRel

        need $ vdir ghcVirtualTarget
               : dir (haddockDocDir bc')
               : map (dir . (targetDir </+>) . osPackageTargetDir) packages

        osTargetAction

buildRules :: Rules ()
buildRules = do
    packageBuildDir PackageWildCard %/> \buildDir -> do
        hpRel <- askHpRelease
        bc <- askBuildConfig
        buildAction buildDir hpRel bc

buildAction :: FilePath -> Release -> BuildConfig -> Action ()
buildAction buildDir hpRel bc = do
        need [ dir sourceDir, vdir ghcVirtualTarget ]

        needsAlex <- usesTool ["//*.x", "//*.lx"]
        needsHappy <- usesTool ["//*.y", "//*.ly"]

        -- These are not *all* the dependencies; just those of the HP
        -- packages, not those from GHC.  depsLibs is just the libraries,
        -- while deps adds any HP packages which provide needed tools.
        depsLibs <- map read <$> readFileLines (packageDepsFile pkg)
        deps <-
            return $ depsLibs ++ needsAlex ?: [alexVer]
                              ++ needsHappy ?: [happyVer]

        putNormal $ show pkg ++ " needs " ++ unwords (map show deps)
        need $ map (dir . packageBuildDir) deps

        putNormal $ ">>> Building " ++ show pkg

        command_ [] "cp" ["-pR", sourceDir, buildDir]

        ghcPkgVerbosity <- shakeToGhcPkgVerbosity
        removeDirectoryRecursive depsDB
        localCommand' [] "ghc-pkg" ["init", depsDB]
        forM_ deps $ \d -> do

            let targetconf = packageInplaceConf d
            hasTargetconf <- doesFileExist targetconf
            when hasTargetconf $
                localCommand' [] "ghc-pkg"
                    [ "register"
                    , "--package-db=" ++ depsDB
                    , "--verbose=" ++ show ghcPkgVerbosity
                    , targetconf
                    ]

        cabalVerbosity <- show . fromEnum <$> shakeToCabalVerbosity
        let cabal c as = localCommand' [Cwd buildDir] "cabal" $
                             c : ("--verbose=" ++ cabalVerbosity) : as
        when (not isAlexOrHappy) $
            cabal "clean" []  -- This is a hack to handle when packages, other
                              -- than alex or happy themselves, have outdated
                              -- bootstrap files in their sdist tarballs.
        cabal "configure" $ confOpts needsAlex needsHappy
        cabal "build" []
        cabal "register"
            ["--gen-pkg-config=" ++ packageTargetConf pkg `relativeToDir` buildDir]
        osPackagePostRegister pkg
        cabal "register"
            ["--inplace"
            , "--gen-pkg-config=" ++ packageInplaceConf pkg `relativeToDir` buildDir]
        stripAbiDepends (packageInplaceConf pkg)
        stripAbiDepends (packageTargetConf pkg)
        cReadArgs <- map (haddockReadArg . osGhcPkgPathMunge pkgHtmlDir)
                     <$> haddockAllCorePkgLocs hpRel bc
        pReadArgs <- map (haddockReadArg . osPlatformPkgPathMunge pkgHtmlDir)
                     <$> haddockPlatformPkgLocs depsLibs
        cabal "haddock" $
            [ "--hyperlink-source"
            , "--hoogle"
            , "--html"
            , "--with-haddock=" ++ haddockExe `relativeToDir` buildDir
            ]
            ++ map (\s -> "--haddock-option=" ++ s) (cReadArgs ++ pReadArgs)

  where
    OS{..} = osFromConfig bc

    pkg = extractPackage buildDir
    sourceDir = packageSourceDir pkg
    depsDB = packageDepsDB pkg

    isAlexOrHappy = pkgName pkg `elem` ["alex", "happy"]
    usesTool pats =
        if not isAlexOrHappy
            then (not . null) <$> getDirectoryFiles sourceDir pats
            else return False   -- don't depend on yourself!

    happyExe = packageBuildDir happyVer </> "dist/build/happy/happy"
    happyTemplateDir = packageBuildDir happyVer
    alexExe = packageBuildDir alexVer </> "dist/build/alex/alex"
    haddockExe = ghcLocalDir </> "bin/haddock"
    haveCabalInstall = False
    cabalInstallDir = undefined
    doProfiling = True
    doShared = osDoShared

    confOpts needsAlex needsHappy =
        osPackageConfigureExtraArgs pkg
        ++ map ("--package-db="++) [ "clear", "global", "../package.conf.d" ]
        ++ needsAlex ?: [ "--with-alex=" ++ alexExe `relativeToDir` buildDir ]
        ++ needsHappy ?: [ "--with-happy=" ++ happyExe `relativeToDir` buildDir
                         , "--happy-options=--template=" ++ happyTemplateDir `relativeToDir` buildDir
                         ]
        ++ haveCabalInstall ?: [ "--with-cabal-install=" ++ cabalInstallDir ]
        ++ doProfiling ?: [ "--enable-library-profiling" ]
        ++ doShared ?: [ "--enable-shared" ]

    b ?: l = if b then l else []

    alexVer = findPackage "alex"
    happyVer = findPackage "happy"
    findPackage s = case filter ((==s) . pkgName) . allPackages (bcIncludeExtra bc) $ hpRel of
        (p:_) -> p
        [] -> error $ "Can't find needed package " ++ s ++ " in HP."

    pkgHtmlDir = osPkgHtmlDir pkg



installRules :: BuildConfig -> Rules ()
installRules bc = do
    targetDir </+> osPackageTargetDir PackageWildCard %/> \targetPkgDir -> do
        let pkg = read $ takeFileName targetPkgDir :: Package
        let buildDir = packageBuildDir pkg
        need [ dir buildDir ]
        -- The reason to "copy" rather than "install" is to avoid actually
        -- executing anything from within the targetDir, thus keeping it
        -- pristine, just as it will be for the actual end-user when first
        -- installed.
        localCommand' [Cwd buildDir]
            "cabal" ["copy", "--destdir=" ++ targetDir `relativeToDir` buildDir]
        osPackageInstallAction pkg
  where
    OS{..} = osFromConfig bc

stripAbiDepends :: FilePath -> Action ()
stripAbiDepends f = do
  hasF <- doesFileExist f
  when hasF $ liftIO $ do
    ls <- lines <$> readFile f
    _ <- evaluate $ length ls
    let (l1,l2) = break ("abi-depends:" `isPrefixOf`) ls
    writeFile f $ unlines $ l1 ++ (dropWhile (all isSeparator . take 1) (drop 1 l2))