{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards #-}

module Config
    ( askHp
    , askHpVersion

    , GhcConfig(..)
    , askGhcConfig
    , askGhcBinDistTarFile

    , addConfigOracle
    )
    where

import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Version (Version)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath

import PlatformDB
import Utils (version)


-- | The configuration of GHC used for this build.
-- This is entirely specified by the GHC bindist used, and in turn determines
-- the configuration of the HP build.
data GhcConfig = GhcConfig
    { ghcBinDistTarFile :: FilePath
    , ghcVersion :: Version
    , ghcArch :: String             -- ex.: "arm", "i386", "x86_64", etc.
    , ghcOsVendor :: String         -- ex.: "apple", "solaris", "unknown"
    , ghcOs :: String               -- ex.: "freebsd", "linux", "darwin"
    , ghcOsDistribution :: String   -- ex.: "deb7", "mavericks"
    }
  deriving (Show)

fromBinDistTarFile :: FilePath -> GhcConfig
fromBinDistTarFile fp =
    if ok then GhcConfig {..}
          else error $ "fromBinDistTarFile name unparseable: " ++ base
  where
    ghcBinDistTarFile = fp
        -- example: ghc-7.8.0.20140228-x86_64-apple-darwin-lion.tar.bz2
    base0 = dropExtension $ takeFileName fp
    base = (if takeExtension base0 == ".tar" then dropExtension else id) base0
    parts = splitOn "-" base
    (prefix : verStr : ghcArch : ghcOsVendor : ghcOs : remainder) = parts
    ghcVersion = version verStr
    ghcOsDistribution = intercalate "-" remainder
    ok = (length parts >= 5) && (prefix == "ghc")

{-
Release is not used directly in the oracles because writing all the required
instances is possible but lengthly, and Version is missing instances for both
Hashable and Binary. It is easier to just rely on the generated instances of
Show and Read for, and use String in the oracle.
-}

newtype PlatformQ = PlatformQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | Provide the Platform release information
-- The release information will be tracked as a dependency

askHp :: Action Release
askHp = read <$> askOracle (PlatformQ ())


newtype PlatformVersionQ = PlatformVersionQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | Provide the Platform version
-- The version number will be tracked as a dependency

askHpVersion :: Action Release
askHpVersion = read <$> askOracle (PlatformVersionQ ())


-- | Provide the GHC configuration.
-- This configuration will be tracked as a dependency.
askGhcConfig :: Action GhcConfig
askGhcConfig = fromBinDistTarFile <$> askGhcBinDistTarFile


newtype BinDistTarFileQ = BinDistTarFileQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | Provide the bindist tarfile.
-- The filepath will be tracked as a dependency.
askGhcBinDistTarFile :: Action FilePath
askGhcBinDistTarFile = do
    tarFile <- askOracle $ BinDistTarFileQ ()
    need [tarFile]
    return tarFile




addConfigOracle :: Release -> FilePath -> Rules ()
addConfigOracle hp tarfile = do
    _ <- addOracle $
            \(PlatformQ _) -> return $ show hp
    _ <- addOracle $
            \(PlatformVersionQ _) -> return $ show $ relVersion hp
    _ <- addOracle $
            \(BinDistTarFileQ _) -> return tarfile
    return ()


