{-# LANGUAGE CPP #-}

module Package
    ( packageRules
    )
  where

import Control.Applicative ((<$>))
import Data.Graph (flattenSCCs, stronglyConnComp)
import Data.List (isPrefixOf, isInfixOf)
#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down(..))
#endif
import Data.Version (showVersion)
import Development.Shake
import Development.Shake.FilePath

import Config
import Dirs
import LocalCommand
import Paths
import PlatformDB
import Types
import Utils

#if !MIN_VERSION_base(4,6,0)
newtype Down a = Down a deriving (Eq)
instance Ord a => Ord (Down a) where
    compare (Down x) (Down y) = y `compare` x
#endif

-- | All the default rules for packages. Includes unpacking, computing the
-- dependencies, and building.

packageRules :: Rules ()
packageRules = do
    packageSourceDir PackageWildCard %/> \srcDir -> do
        let pkg = extractPackage srcDir
        let destDir = takeDirectory srcDir
        let providedDir = sourceForPackageDir pkg
        provided <- doesDirectoryExist providedDir
        if provided
            then command_ [] "cp" ["-pR", providedDir, srcDir]
            else do
                command_ [Cwd destDir] "cabal" ["unpack", show pkg]
                command_ [Cwd destDir] "mv" [show pkg, "source"]

    packageDepsFile PackageWildCard %> \depFile -> do
        hpRel <- askHpRelease
        bc <- askBuildConfig
        installAction depFile (bcIncludeExtra bc) hpRel

    listBuild %> \out -> do
        hpRel <- askHpRelease
        bc <- askBuildConfig
        let pkgs = platformPackages (bcIncludeExtra bc) hpRel
        need $ map packageDepsFile pkgs
        nodes <- mapM buildNode pkgs
        writeFileLinesChanged out $ flattenSCCs $ stronglyConnComp nodes
  where
    buildNode p = do
        ns <- map (pkgName . read) <$> (readFileLines $ packageDepsFile p)
        return (show p, Down $ pkgName p, map Down ns)
            -- Having the package names reverse sort as graph keys makes the SCC
            -- components come out closer to normal sort order. Go figure!


installAction :: FilePath -> Bool -> Release -> Action ()
installAction depFile incExtras hpRel = do
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
            (allPackages incExtras) hpRel

    decode out = case drop 1 $ lines out of
        ("All the requested packages are already installed:":_) ->
            Right []
        ("In order, the following would be installed (use -v for more details):":ls) ->
            let deps = map (head . words) ls
            in if all (`elem` packages) deps
                  || "alex" `isInfixOf` depFile
                  || "happy" `isInfixOf` depFile
                  || any (`isInfixOf` depFile) ["QuickCheck","tf-random"]
                then Right deps
                else Left $ "Depends on non-HP packages: "
                            ++ unwords (filter (not . (`elem` packages)) deps) ++ "\n" ++ "Could not build " ++ depFile
        _ -> Left out

    packages = map show $ (allPackages incExtras) hpRel
