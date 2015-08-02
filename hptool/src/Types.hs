{-# LANGUAGE CPP, RecordWildCards #-}

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
    )
  where

#if MIN_VERSION_base(4,6,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Version (Version, showVersion, parseVersion)
import Development.Shake (Action)
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
    parts <- endBy1 part (char '-')
    ver <- parseVersion
    return $ Package (intercalate "-" parts) ver
  where
#if MIN_VERSION_base(4,6,0)
    part = (:) <$> satisfy lead <*> munch more
#else
    part = do { l <- satisfy lead; m <- munch more; return (l:m) }
#endif

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
    , bcPrefix :: Maybe FilePath   -- ex.: "/usr/local/haskell"
    }
  deriving (Read, Show)

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
