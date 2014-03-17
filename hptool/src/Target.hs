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
import Paths
import PlatformDB
import Types
import Utils


targetRules :: HpVersion -> Rules ()
targetRules hpVer = do
    buildRules
    installRules hpVer
    targetDir ~> do
        hpRel <- askHp
        let hpVer' = relVersion hpRel
            packages = platformPackages hpRel

        need $ dir ghcTargetDir : map (dir . packageTargetDir hpVer') packages


buildRules :: Rules ()
buildRules = do
    packageBuildDir PackageWildCard */> \buildDir -> do
        hpRel <- askHp
        buildAction buildDir hpRel

buildAction :: FilePath -> Release -> Action ()
buildAction buildDir hpRel = do
        let pkg = extractPackage buildDir
        let sourceDir = packageSourceDir pkg
        let depsDB = packageDepsDB pkg

        need [ dir sourceDir ]

        let isAlexOrHappy = pkgName pkg `elem` ["alex", "happy"]
        let usesTool pats =
                if not isAlexOrHappy
                    then (not . null) <$> getDirectoryFiles sourceDir pats
                    else return False   -- don't depend on yourself!
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
    prefix = "/Library/Haskell/" ++ show hpVer ++ "/lib/$pkgid"
    happyExe = packageBuildDir happyVer </> "dist/build/happy/happy"
    happyTemplateDir = packageBuildDir happyVer
    alexExe = packageBuildDir alexVer </> "dist/build/alex/alex"
    haveCabalInstall = False
    cabalInstallDir = undefined
    doProfiling = True
    doShared = True
    hpVer = relVersion hpRel

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




installRules :: HpVersion -> Rules ()
installRules hpVer = do
    packageTargetDir hpVer PackageWildCard */> \targetPkgDir -> do
        let pkg = read $ takeFileName targetPkgDir :: Package
        let buildDir = packageBuildDir pkg
        let confFile = packageTargetConf pkg
        let regDir = registrationTargetDir hpVer
        let regFile = regDir </> show pkg
        need [ dir buildDir ]
        makeDirectory regDir

        let cabal c as = localCommand [Cwd buildDir] "cabal" $ c : as :: Action ()
        cabal "copy" ["--destdir=" ++ targetDir ® buildDir]
        hasReg <- doesFileExist confFile
        if hasReg
            then command_ [] "cp" [confFile, regFile]
            else command_ [] "rm" ["-f", regFile]


