{-# LANGUAGE RecordWildCards #-}

module Types
    ( PackageName
    , Package(..)
    , HpVersion(..)
    , GhcVersion(..)
    , IncludeType(..)
    , Include
    , Release(..)
    , BuildConfig(..)
    )
  where

import Control.Applicative ((<$>), (<*>))
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Version (Version, showVersion, parseVersion)
import Text.ParserCombinators.ReadP (ReadP,
    char, endBy1, munch, readP_to_S, satisfy, skipSpaces)


type PackageName = String


-- | A package identifier: name and specific version.
-- The 'Read' and 'Show' instances use the common format for writing these with
-- a hyphen between the two parts. i.e.: "acme-inator-0.1.0.0". Note that no
-- component of a package name can start with a digit.
data Package = Package { pkgName :: PackageName, pkgVersion :: Version }

readPackageP :: ReadP Package
readPackageP = do
    skipSpaces  -- derived Read instances expect all types to do this!
    parts <- endBy1 ((:) <$> satisfy lead <*> munch more) (char '-')
    ver <- parseVersion
    return $ Package (intercalate "-" parts) ver
  where
    lead c = not (isDigit c) && more c
    more c = c /= '-'

formatPackage :: PackageName -> Version -> String
formatPackage name ver = name ++ '-' : showVersion ver

instance Read Package where
    readsPrec _ = readP_to_S readPackageP
instance Show Package where
    show Package{..} = formatPackage pkgName pkgVersion


-- | How a package is included in the platform.
data IncludeType = IncGHC | IncGHCLib | IncLib | IncTool
                 | IncIfWindows IncludeType
                 | IncIfNotWindows IncludeType
  deriving (Eq, Read, Show)


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
instance Read HpVersion where
    readsPrec _ = readP_to_S $ readFixedPacakgeP HpVersion "haskell-platform"
instance Show HpVersion where
    show HpVersion{..} = formatPackage "haskell-platform" hpVersion

-- | Version of the GHC.
-- 'Read'/'Show' format is "ghc-<version>"
newtype GhcVersion = GhcVersion { ghcVersion :: Version }
instance Read GhcVersion where
    readsPrec _ = readP_to_S $ readFixedPacakgeP GhcVersion "ghc"
instance Show GhcVersion where
    show GhcVersion{..} = formatPackage "ghc" ghcVersion


type Include = (IncludeType, Package)

-- | The definition of a Release of the platform.
data Release = Release
    { relVersion :: HpVersion
    , relIncludes :: [Include]
    }
  deriving (Read, Show)

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
    }
  deriving (Read, Show)
