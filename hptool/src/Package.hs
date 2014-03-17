module Package
    ( packageRules
    )
  where

import Control.Applicative ((<$>))
import Data.Graph (flattenSCCs, stronglyConnComp)
import Data.Version (showVersion)
import Development.Shake
import Development.Shake.FilePath

import Config
import Dirs
import GhcDist
import Paths
import PlatformDB
import Types
import Utils


-- | All the default rules for packages. Includes unpacking, computing the
-- dependencies, and building.

packageRules :: Rules ()
packageRules = do
    packageSourceDir PackageWildCard */> \srcDir -> do
        let pkg = extractPackage srcDir
        let destDir = takeDirectory srcDir
        let providedDir = sourceForPackageDir pkg
        provided <- doesDirectoryExist providedDir
        if provided
            then command_ [] "cp" ["-pR", providedDir, srcDir]
            else do
                command_ [Cwd destDir] "cabal" ["unpack", show pkg]
                command_ [Cwd destDir] "mv" [show pkg, "source"]

    packageDepsFile PackageWildCard *> \depFile -> do
        hpRel <- askHpRelease
        installAction depFile hpRel

    listBuild *> \out -> do
        hpRel <- askHpRelease
        let pkgs = platformPackages hpRel
        need $ map packageDepsFile pkgs
        nodes <- mapM buildNode pkgs
        writeFileLinesChanged out $ flattenSCCs $ stronglyConnComp nodes
  where
    buildNode p = do
        ns <- map (pkgName . read) <$> (readFileLines $ packageDepsFile p)
        return (show p, pkgName p, ns)


installAction :: FilePath -> Release -> Action ()
installAction depFile hpRel = do
    let pkg = extractPackage depFile
    let srcDir = packageSourceDir pkg
    need [ dir srcDir ]

    (Exit _, Stdout out, Stderr err) <- localCommand [Cwd srcDir]
        "cabal" cabalInstallArgs
    case decode out of
        Right deps -> writeFileLinesChanged depFile deps
        Left msg -> error $ "Error computing dependencies of "
                            ++ show pkg ++ ":\n"
                            ++ msg ++ "\n"
                            ++ err
  where
    cabalInstallArgs =
        [ "install"
        , "--dry-run"
        , "--only-dependencies"
        , "--package-db=clear"
        , "--package-db=global"
        ] ++ map ("--constraint=" ++) constraints

    constraints =
        map (\p -> pkgName p ++ "==" ++ showVersion (pkgVersion p)) $
            allPackages hpRel

    decode out = case drop 1 $ lines out of
        ("All the requested packages are already installed:":_) ->
            Right []
        ("In order, the following would be installed (use -v for more details):":ls) ->
            let deps = map (head . words) ls
            in if all (`elem` packages) deps
                then Right deps
                else Left $ "Depends on non-HP packages: "
                            ++ unwords (filter (not . (`elem` packages)) deps)
        _ -> Left out

    packages = map show $ allPackages hpRel

