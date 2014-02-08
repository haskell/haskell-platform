{-# LANGUAGE RecordWildCards #-}

module Paths
    ( buildRoot

    , Package(..), includeToPackage
    , packageSourceDir, packageBuildDir, packageDepsFile
    , packageDepsDB, packageInplaceConf, packageTargetConf
    , extractPackage

    , ghcBinDistDir, ghcLocalDir

    , Hp(..), releaseToHp
    , hpSourceDir
    , hpSourceEtcDir, hpSourcePackagesDir, hpSourceToolDir
    , hpSourcePackageDir

    , listsDir
    , listBuild, listCore, listSource

    , targetDir
    , ghcTargetDir, hpTargetDir, packageTargetDir
    , registrationTargetDir

    , productDir

    , sourceRoot
    , sourceForPackageDir

    , markerRoot
    )
  where

import Control.Applicative ((<$>), (<*>))
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Version (Version, showVersion, parseVersion)
import Development.Shake.FilePath
import Text.ParserCombinators.ReadP (char, endBy1, munch, readP_to_S, satisfy)

import PlatformDB (Include(..), Release(..))


-- | All work of the build is in here
buildRoot :: FilePath
buildRoot = "build"


data Package = Package { pkgName :: String, pkgVersion :: Version }
             | PackageWildCard

instance Show Package where
    show Package{..} = pkgName ++ '-' : showVersion pkgVersion
    show PackageWildCard = "*"

instance Read Package where
    readsPrec _ = readP_to_S $ do
        parts <- endBy1 ((:) <$> satisfy lead <*> munch more) (char '-')
        ver <- parseVersion
        return $ Package (intercalate "-" parts) ver
      where
        lead c = not (isDigit c) && more c
        more c = c /= '-'

includeToPackage :: Include -> Package
includeToPackage Include{..} = Package incPackage incVersion


packageBase :: FilePath -> Package -> FilePath
packageBase part p = buildRoot </> "package" </> show p </> part


packageSourceDir, packageBuildDir, packageDepsFile,
    packageDepsDB, packageInplaceConf, packageTargetConf
        :: Package -> FilePath

packageSourceDir    = packageBase "source"          -- package source
packageBuildDir     = packageBase "build"           -- copy for building
packageDepsFile     = packageBase "deps"            -- packages this depends on
packageDepsDB       = packageBase "packages.conf.d" -- db for use during build
packageInplaceConf  = packageBase "inplace.conf"    -- local reg. for building
packageTargetConf   = packageBase "target.conf"     -- reg. for target inst.

extractPackage :: FilePath -> Package
extractPackage = read . takeFileName . takeDirectory

ghcBinDistDir :: Version -> FilePath
ghcBinDistDir ver = buildRoot </> "ghc-bindist" </> ghcVer
  where
    ghcVer = "ghc-" ++ showVersion ver

ghcLocalDir :: FilePath
ghcLocalDir = buildRoot </> "ghc-bindist" </> "local"


data Hp = Hp { hpVersion :: Version }

releaseToHp :: Release -> Hp
releaseToHp Release{..} = Hp relVersion

hpSourceDir :: Hp -> FilePath
hpSourceDir Hp{..} = buildRoot </> "src-tarball" </> hpVer
  where
    hpVer = "haskell-platform-" ++ showVersion hpVersion

hpSourceBase :: FilePath -> Hp -> FilePath
hpSourceBase part hp = hpSourceDir hp </> part

hpSourceEtcDir, hpSourcePackagesDir, hpSourceToolDir :: Hp -> FilePath
hpSourceEtcDir      = hpSourceBase  "etc"
hpSourcePackagesDir = hpSourceBase "packages"
hpSourceToolDir     = hpSourceBase "hptool"

hpSourcePackageDir :: Hp -> Package -> FilePath
hpSourcePackageDir hp p = hpSourcePackagesDir hp </> show p


listsDir :: FilePath
listsDir = buildRoot </> "lists"

listBuild, listCore, listSource :: FilePath
listBuild  = listsDir </> "platform.packages"
listCore   = listsDir </> "core.packages"
listSource = listsDir </> "source.packages"

targetDir :: FilePath
targetDir = buildRoot </> "target"

ghcTargetDir :: FilePath
ghcTargetDir = targetDir </> "Library/Frameworks/GHC.framework"

hpTargetDir :: Hp -> FilePath
hpTargetDir hp = targetDir </> "Library/Haskell" </> showVersion (hpVersion hp)

packageTargetDir :: Hp -> Package -> FilePath
packageTargetDir hp p = hpTargetDir hp </> "lib" </> show p

registrationTargetDir :: Hp -> FilePath
registrationTargetDir hp = hpTargetDir hp </> "lib" </> "registrations"


-- | Final products are placed here
productDir :: FilePath
productDir = buildRoot </> "product"




-- | Sources intended to override those up on hackage go here
sourceRoot :: FilePath
sourceRoot = "packages"

sourceForPackageDir :: Package -> FilePath
sourceForPackageDir p = sourceRoot </> show p


markerRoot :: String
markerRoot = buildRoot </> ".markers"



