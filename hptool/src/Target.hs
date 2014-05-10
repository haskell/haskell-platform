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
import GhcDist
import HaddockMaster
import OS
import Paths
import PlatformDB
import Types
import Utils


targetRules :: BuildConfig -> Rules ()
targetRules bc = do
    buildRules
    installRules bc
    targetDir ~> do
        hpRel <- askHpRelease
        bc' <- askBuildConfig
        let packages = platformPackages hpRel
            packageTargetDir = osPackageTargetDir $ osFromConfig bc'

        need $ vdir ghcVirtualTarget
               : dir (haddockDocDir bc')
               : map (dir . (targetDir </+>) . packageTargetDir) packages


buildRules :: Rules ()
buildRules = do
    packageBuildDir PackageWildCard */> \buildDir -> do
        hpRel <- askHpRelease
        bc <- askBuildConfig
        buildAction buildDir hpRel bc

buildAction :: FilePath -> Release -> BuildConfig -> Action ()
buildAction buildDir hpRel bc = do
        need [ dir sourceDir ]

        needsAlex <- usesTool ["//*.x", "//*.lx"]
        needsHappy <- usesTool ["//*.y", "//*.ly"]

        deps <- do
            deps0 <- map read <$> readFileLines (packageDepsFile pkg)
            return $ deps0 ++ needsAlex ?: [alexVer]
                           ++ needsHappy ?: [happyVer]

        putNormal $ show pkg ++ " needs " ++ unwords (map show deps)
        need $ map (dir . packageBuildDir) deps

        putNormal $ ">>> Building " ++ show pkg

        command_ [] "cp" ["-pR", sourceDir, buildDir]

        removeDirectoryRecursive depsDB
        localCommand' [] "ghc-pkg" ["init", depsDB]
        forM_ deps $ \d -> do
            let inplace = packageInplaceConf d
            hasInplace <- doesFileExist inplace
            when hasInplace $
                localCommand' [] "ghc-pkg"
                    [ "register"
                    , "--package-db=" ++ depsDB
                    , inplace
                    ]

        let cabal c as = localCommand' [Cwd buildDir] "cabal" $ c : as
        when (not isAlexOrHappy) $
            cabal "clean" []  -- This is a hack to handle when packages, other
                              -- than alex or happy themselves, have outdated
                              -- bootstrap files in their sdist tarballs.
        cabal "configure" $ confOpts needsAlex needsHappy
        cabal "build" []
        cabal "register"
            ["--gen-pkg-config=" ++ packageTargetConf pkg ® buildDir]
        cabal "register"
            ["--inplace"
            , "--gen-pkg-config=" ++ packageInplaceConf pkg ® buildDir]
        cabal "haddock" ["--hyperlink-source"]  -- TODO(mzero): make optional

  where
    pkg = extractPackage buildDir
    sourceDir = packageSourceDir pkg
    depsDB = packageDepsDB pkg

    isAlexOrHappy = pkgName pkg `elem` ["alex", "happy"]
    usesTool pats =
        if not isAlexOrHappy
            then (not . null) <$> getDirectoryFiles sourceDir pats
            else return False   -- don't depend on yourself!

    packageTargetDir = osPackageTargetDir $ osFromConfig bc

    prefix = packageTargetDir pkg
    happyExe = packageBuildDir happyVer </> "dist/build/happy/happy"
    happyTemplateDir = packageBuildDir happyVer
    alexExe = packageBuildDir alexVer </> "dist/build/alex/alex"
    haveCabalInstall = False
    cabalInstallDir = undefined
    doProfiling = True
    doShared = True

    confOpts needsAlex needsHappy =
        [ "--prefix=" ++ prefix ]
        ++ [ "--libsubdir=", "--datasubdir=", "--docdir=$prefix/doc" ]
        ++ map ("--package-db="++) [ "clear", "global", "../packages.conf.d" ]
        ++ needsAlex ?: [ "--with-alex=" ++ alexExe ® buildDir ]
        ++ needsHappy ?: [ "--with-happy=" ++ happyExe ® buildDir
                         , "--happy-options=--template=" ++ happyTemplateDir ® buildDir
                         ]
        ++ haveCabalInstall ?: [ "--with-cabal-install=" ++ cabalInstallDir ]
        ++ doProfiling ?: [ "--enable-library-profiling" ]
        ++ doShared ?: [ "--enable-shared" ]

    b ?: l = if b then l else []

    alexVer = findPackage "alex"
    happyVer = findPackage "happy"
    findPackage s = case filter ((==s) . pkgName) . allPackages $ hpRel of
        (p:_) -> p
        [] -> error $ "Can't find needed package " ++ s ++ " in HP."




installRules :: BuildConfig -> Rules ()
installRules bc = do
    targetDir </+> osPackageTargetDir PackageWildCard */> \targetPkgDir -> do
        let pkg = read $ takeFileName targetPkgDir :: Package
        let buildDir = packageBuildDir pkg
        need [ dir buildDir ]
        localCommand' [Cwd buildDir]
            "cabal" ["copy", "--destdir=" ++ targetDir ® buildDir]
        osPackageInstallAction pkg
  where
    OS{..} = osFromConfig bc

