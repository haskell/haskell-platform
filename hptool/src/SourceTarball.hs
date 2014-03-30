module SourceTarball where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.Text.Lazy.IO as TL
import Data.Version (showVersion)
import Development.Shake
import Development.Shake.FilePath
import Text.Hastache
import Text.Hastache.Context

import Config
import Dirs
import GhcDist
import Paths
import PlatformDB
import Types
import Utils

sourceTarballRules :: FilePath -> Rules ()
sourceTarballRules srcTarFile = do
    packageListRule listCore corePackages
    packageListRule listSource platformPackages
    cabalFileRule

    srcTarFile *> \out -> do
        hpRelease <- askHpRelease
        tarFileAction out hpRelease
  where
    packageListRule target pkgFn =
        target *> \out -> do
            hpRelease <- askHpRelease
            let pkgs = pkgFn hpRelease
            writeFileLinesChanged out (map show pkgs)


tarFileAction :: FilePath -> Release -> Action ()
tarFileAction out hpRelease = do
    need $ concat
        [ map (dir . packageSourceDir) sources
        , lists
        , [hpCabalFile, ghcLocalDir]
        ]

    removeDirectoryRecursive topDir
    makeDirectory topDir
    makeDirectory etcDir
    makeDirectory packagesDir

    forM_ sources $ \sPkg ->
        command_ [] "cp"
            [ "-pR"
            , packageSourceDir sPkg
            , hpSourcePackageDir hp sPkg]

    forM_ lists $ \l -> do
        let dest = etcDir </> takeFileName l
        putLoud $ "copyFile' " ++ l ++ " " ++ dest
        copyFile' l dest

    copyFile' hpCabalFile $ topDir </> takeFileName hpCabalFile

    localCommand' [Cwd hptoolSourceDir]
        "cabal" [ "sdist"
                , "--output-directory=" ++ hptoolDistDir ® hptoolSourceDir ]

    command_ [Cwd upDir]
        "tar" ["czf", out ® upDir, takeFileName topDir]
  where
    hp = relVersion hpRelease

    upDir = takeDirectory topDir
    topDir = hpSourceDir hp
    etcDir = hpSourceEtcDir hp
    packagesDir = hpSourcePackagesDir hp
    hptoolSourceDir = "hptool"
    hptoolDistDir = topDir </> "hptool"

    lists = [listBuild, listCore, listSource]

    sources = platformPackages hpRelease


cabalFileRule :: Rules ()
cabalFileRule =
    hpCabalFile *> \cFile -> do
        ctx <- mu <$> askHpRelease
        liftIO $ hastacheFile conf cabalTemplate ctx >>= TL.writeFile cFile
  where
    conf = defaultConfig { muEscapeFunc = emptyEscape }
    cabalTemplate = "hptool/templates/haskell-platform.cabal.mu" -- FIXME
    mu rel = mkStrContext ex
      where
        ex "hpVersion" = MuVariable . showVersion . hpVersion . relVersion $ rel
        ex "ghcVersion" = case pkgsThat [isGhc, not . isLib] of
            [] -> error "No ghc version spec'd in release."
            [ghcPkg] -> MuVariable . showVersion . pkgVersion $ ghcPkg
            _ -> error "More than one ghc version spec'd in release."

        ex "ghcLibs"           = exPkgs $ pkgsThat [isGhc, isLib]
        ex "nonWindowsLibs"    = exPkgs $ pkgsThat [isNotWindows]
        ex "onlyWindowsLibs"   = exPkgs $ pkgsThat [isWindows]
        ex "platformLibs"      = exPkgs $ pkgsThat [not . isGhc, isLib]
        ex "tools"             = exPkgs $ pkgsThat [isTool]

        ex t = error $ "ex: unexpected template tag " ++ t

        pkgsThat tests = packagesByIncludeFilter (\i -> all ($i) tests) rel

        exPkgs pkgs = MuList $ map mkStrContext $ zipMarkLast exPkg pkgs

        zipMarkLast _ [] = []
        zipMarkLast f [p] = [f True p]
        zipMarkLast f (p:ps) = f False p : zipMarkLast f ps

        exPkg _ p "name" = MuVariable $ pad 30 $ pkgName p
        exPkg _ p "version" = MuVariable $ showVersion $ pkgVersion p
        exPkg c _ "comma" = MuVariable $ if c then "" else ","
        exPkg _ _ t = error $ "exPkg: unexpected template tag " ++ t

        pad n s = s ++ replicate (n - length s) ' '
