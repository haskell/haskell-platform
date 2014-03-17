{-# LANGUAGE RecordWildCards #-}

module Paths
    ( buildRoot

    , packageSourceDir, packageBuildDir, packageDepsFile
    , packageDepsDB, packageInplaceConf, packageTargetConf
    , extractPackage
    , PackageWildCard(..), PackagePattern

    , ghcBinDistDir, ghcLocalDir

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

import Development.Shake.FilePath

import Types

-- | All work of the build is in here
buildRoot :: FilePath
buildRoot = "build"



data PackageWildCard = PackageWildCard

class PackagePattern a where
  packagePattern :: a -> FilePath

instance PackagePattern Package where
  packagePattern = show

instance PackagePattern PackageWildCard where
  packagePattern PackageWildCard = "*"




packageBase :: (PackagePattern p) => FilePath -> p -> FilePath
packageBase part p = buildRoot </> "package" </> packagePattern p </> part


packageSourceDir, packageBuildDir, packageDepsFile,
    packageDepsDB, packageInplaceConf, packageTargetConf
        :: (PackagePattern p) => p -> FilePath

packageSourceDir    = packageBase "source"          -- package source
packageBuildDir     = packageBase "build"           -- copy for building
packageDepsFile     = packageBase "deps"            -- packages this depends on
packageDepsDB       = packageBase "packages.conf.d" -- db for use during build
packageInplaceConf  = packageBase "inplace.conf"    -- local reg. for building
packageTargetConf   = packageBase "target.conf"     -- reg. for target inst.

extractPackage :: FilePath -> Package
extractPackage = read . takeFileName . takeDirectory

ghcBinDistDir :: GhcVersion -> FilePath
ghcBinDistDir ghcVer = buildRoot </> "ghc-bindist" </> show ghcVer

ghcLocalDir :: FilePath
ghcLocalDir = buildRoot </> "ghc-bindist" </> "local"



hpSourceDir :: HpVersion -> FilePath
hpSourceDir hpVer = buildRoot </> "src-tarball" </> show hpVer

hpSourceBase :: FilePath -> HpVersion -> FilePath
hpSourceBase part hp = hpSourceDir hp </> part

hpSourceEtcDir, hpSourcePackagesDir, hpSourceToolDir :: HpVersion -> FilePath
hpSourceEtcDir      = hpSourceBase  "etc"
hpSourcePackagesDir = hpSourceBase "packages"
hpSourceToolDir     = hpSourceBase "hptool"

hpSourcePackageDir :: (PackagePattern p) => HpVersion -> p -> FilePath
hpSourcePackageDir hp p = hpSourcePackagesDir hp </> packagePattern p


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

hpTargetDir :: HpVersion -> FilePath
hpTargetDir hpVer = targetDir </> "Library/Haskell" </> show hpVer

packageTargetDir :: (PackagePattern p) => HpVersion -> p -> FilePath
packageTargetDir hpVer p = hpTargetDir hpVer </> "lib" </> packagePattern p

registrationTargetDir :: HpVersion -> FilePath
registrationTargetDir hpVer = hpTargetDir hpVer </> "lib" </> "registrations"


-- | Final products are placed here
productDir :: FilePath
productDir = buildRoot </> "product"




-- | Sources intended to override those up on hackage go here
sourceRoot :: FilePath
sourceRoot = "packages"

sourceForPackageDir :: Package -> FilePath
sourceForPackageDir p = sourceRoot </> show p


markerRoot :: FilePath
markerRoot = buildRoot </> ".markers"



