{-# LANGUAGE CPP, RecordWildCards, DeriveAnyClass, DeriveGeneric #-}

module Types
    ( PackageName
    , Package(..)
    , HaddockPkgLoc(..)
    , HpVersion(..)
    , GhcVersion(..)
    , IncludeType(..)
    , Include
    , Release(..)
    , BuildConfig(..)
    , GhcInstallAction
    , GhcInstall(..)
    , BuildFlavor(..)
    , UserConfig(..)
    )
  where

import Data.Char (isDigit)
import Data.List (intercalate, sort)
import Data.Version (Version, showVersion, parseVersion)
import Development.Shake (Action)
import Development.Shake.Classes (Binary, Hashable, NFData)
import GHC.Generics
import Text.ParserCombinators.ReadP (ReadP,
    char, endBy1, munch, readP_to_S, satisfy, skipSpaces)


type PackageName = String


-- | A package identifier: name and specific version.
-- The 'Read' and 'Show' instances use the common format for writing these with
-- a hyphen between the two parts. i.e.: "acme-inator-0.1.0.0". Note that no
-- component of a package name can start with a digit.
data Package = Package { pkgName :: PackageName, pkgVersion :: Version }
  deriving (Eq, Ord, Generic, Hashable, Binary, NFData)

readPackageP :: ReadP Package
readPackageP = do
    skipSpaces  -- derived Read instances expect all types to do this!
    parts <- endBy1 part (char '-')
    ver <- parseVersion
    return $ Package (intercalate "-" parts) ver
  where
    part = (:) <$> satisfy lead <*> munch more

    lead c = not (isDigit c) && more c
    more c = c /= '-'

formatPackage :: PackageName -> Version -> String
formatPackage name ver = name ++ '-' : showVersion ver

instance Read Package where
    readsPrec _ = readP_to_S readPackageP
instance Show Package where
    show Package{..} = formatPackage pkgName pkgVersion


-- | How a package is included in the platform.
data IncludeType = IncGHC | IncGHCLib | IncGHCTool | IncLib | IncTool
                 | IncIfWindows IncludeType
                 | IncIfNotWindows IncludeType
  deriving (Eq, Read, Show, Ord, Generic, Hashable, Binary, NFData)


readFixedPacakgeP :: (Version -> a) -> String -> ReadP a
readFixedPacakgeP f fixedName = do
    Package{..} <- readPackageP
    if pkgName == fixedName
        then return $ f pkgVersion
        else fail $ "readFixedPacakgeP: expected " ++ fixedName
                    ++ ", found " ++ pkgName

-- | Version of the platform itself.
-- 'Read'/'Show' format is "haskell-platform-<version>"
newtype HpVersion = HpVersion { hpVersion :: Version }
  deriving (Eq, Generic, Hashable, Binary, NFData)
instance Read HpVersion where
    readsPrec _ = readP_to_S $ readFixedPacakgeP HpVersion "haskell-platform"
instance Show HpVersion where
    show HpVersion{..} = formatPackage "haskell-platform" hpVersion

-- | Version of the GHC.
-- 'Read'/'Show' format is "ghc-<version>"
newtype GhcVersion = GhcVersion { ghcVersion :: Version }
  deriving (Eq, Generic, Hashable, Binary, NFData)
instance Read GhcVersion where
    readsPrec _ = readP_to_S $ readFixedPacakgeP GhcVersion "ghc"
instance Show GhcVersion where
    show GhcVersion{..} = formatPackage "ghc" ghcVersion


type Include = (IncludeType, Package)

-- | The definition of a Release of the platform.
data Release = Release
    { relVersion :: HpVersion
    , relMinimalIncludes :: [Include]
    , relIncludes :: [Include]
    }
  deriving (Read, Show, Generic, Hashable, Binary, NFData)
-- The order of entries in the relMinimalIncludes and relIncludes does not
-- matter for equality (and these lists will have 20-40 entries max with no
-- duplicates), so this Eq instance addresses that.
instance Eq Release where
    (Release v1 m1 i1) == (Release v2 m2 i2) =
        (v1 == v2) && (sortedEq m1 m2) && (sortedEq i1 i2)
        where sortedEq as bs = (sort as) == (sort bs)

-- | The configuration of a build. These are the parameters of the build that
-- specify what type of system this build of Haskell Platform is for. It
-- includes architecture and OS information. It also includes the versions of
-- HP and GHC used, as on most platforms, these versions used in the formation
-- of where on the target system the various components are located.
data BuildConfig = BuildConfig
    { bcHpVersion :: HpVersion
    , bcGhcVersion :: GhcVersion
    , bcArch :: String             -- ex.: "arm", "i386", "x86_64", etc.
    , bcOsVendor :: String         -- ex.: "apple", "solaris", "unknown"
    , bcOs :: String               -- ex.: "freebsd", "linux", "darwin"
    , bcOsDistribution :: String   -- ex.: "deb7", "mavericks"
    , bcPrefix :: Maybe FilePath   -- ex.: "/usr/local/haskell"
    , bcIncludeExtra :: Bool
    }
  deriving (Read, Show, Eq, Generic, Hashable, Binary, NFData)

-- | A function that is used for the actions after untar-ing GHC.
-- The build configuration and file path of the untar-ed directory is
-- provided, and the file path of the installed directory is returned.
type GhcInstallAction = BuildConfig -> FilePath -> Action (Maybe FilePath)

-- | After untar-ing GHC, some platforms require configure-make, while
-- other platforms need other custom steps.
data GhcInstall =
    GhcInstallConfigure
  | GhcInstallCustom GhcInstallAction

-- | Info retrieved from the package conf files: the first is
-- the haddock-html field; the second is the haddock-interfaces field.
data HaddockPkgLoc = HaddockPkgLoc { pkgLocHtml, pkgLocIntf :: String }
    deriving (Show)

-- | The HP can be built in two flavors: full or core, where "full" is
-- "all the batteries included" (the full, agreed upon set of packages felt to
-- be most useful to many GHC users); and "core" is just GHC, the packages
-- included with its release (and minimally needed to successfully compile and
-- link).
data BuildFlavor = BuildFlavorCore | BuildFlavorFull
    deriving (Eq, Show)

-- | The digested user-provided flags and arguments for the build.
-- Used for setting the user requests into the "oracle".
data UserConfig = UserConfig
    { ucPrefix        :: Maybe FilePath
    , ucBuildFlavor   :: BuildFlavor
    , ucCabalExe      :: FilePath
    , ucGHCBinDist    :: FilePath
    , ucStackExe      :: FilePath
    , ucGHCUsersPDF   :: FilePath
    , ucGHCUsersHTML  :: FilePath
    , ucGHCLibsHTML   :: FilePath
    , ucHaddockHTML   :: FilePath
    }
    deriving (Show)
