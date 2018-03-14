{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             RecordWildCards #-}

module Config
    ( askHpRelease
    , askGhcBinDistTarFile
    , askBuildConfig
    , addConfigOracle
    , askCabalExe
    , askStackExe
    , askGhcUgPDF
    , askGhcUgHtml
    , askGhcLibs
    , askHaddockHTML
    )
    where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath

import Types
import Utils (readMaybe, version)


readOracle :: (ShakeValue q, Read a) => String -> q -> Action a
readOracle name q = do
    s <- askOracle q
    maybe (error $ msg s) return $ readMaybe reads s
  where
    msg s = "readOracle failed to parse " ++ name ++ " from " ++ show s

{-
Release and BuildConfig are not used directly in the oracles because writing all
the required instances is possible but lengthly, and Version is missing
instances for both Hashable and Binary. It is easier to just rely on the
generated instances of Show and Read for these, and use String in the oracle.
-}

newtype HpReleaseQ = HpReleaseQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | Provide the Platform release information
-- The release information will be tracked as a dependency

askHpRelease :: Action Release
askHpRelease = readOracle "HpRelease" (HpReleaseQ ())


newtype GhcBinDistTarFileQ = GhcBinDistTarFileQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | Provide the bindist tar file.
-- The filepath will be tracked as a dependency.
askGhcBinDistTarFile :: Action FilePath
askGhcBinDistTarFile = do
    tarFile <- askOracle $ GhcBinDistTarFileQ ()
    need [tarFile]
    return tarFile


newtype BuildConfigQ = BuildConfigQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | Provide the Platform release information
-- The release information will be tracked as a dependency

askBuildConfig :: Action BuildConfig
askBuildConfig = readOracle "BuildConfig" (BuildConfigQ ())

newtype StackExeQ = StackExeQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

newtype CabalExeQ = CabalExeQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | Provide the stack executable
-- The filepath will be tracked as a dependency
askStackExe :: Action FilePath
askStackExe = do
    stackexe <- askOracle $ StackExeQ ()
    need [stackexe]
    return stackexe

-- | Provide the stack executable
-- The filepath will be tracked as a dependency
askCabalExe :: Action FilePath
askCabalExe = do
    cabalexe <- askOracle $ CabalExeQ ()
    need [cabalexe]
    return cabalexe

newtype GhcUsersGuidePDFFileQ = GhcUsersGuidePDFFileQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

askGhcUgPDF :: Action FilePath
askGhcUgPDF = do
  fname <- askOracle $ GhcUsersGuidePDFFileQ ()
  need [fname]
  return fname

newtype GhcUsersGuideHTMLTarFileQ = GhcUsersGuideHTMLTarFileQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

askGhcUgHtml :: Action FilePath
askGhcUgHtml = do
  fname <- askOracle $ GhcUsersGuideHTMLTarFileQ ()
  need [fname]
  return fname

newtype GhcLibsHTMLTarFileQ =  GhcLibsHTMLTarFileQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

askGhcLibs :: Action FilePath
askGhcLibs = do
  fname <- askOracle $ GhcLibsHTMLTarFileQ ()
  need [fname]
  return fname

newtype HaddockHTMLTarFileQ = HaddockHTMLTarFileQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

askHaddockHTML :: Action FilePath
askHaddockHTML = do
  fname <- askOracle $ HaddockHTMLTarFileQ ()
  need [fname]
  return fname

addConfigOracle :: Release -> UserConfig -> Rules BuildConfig
addConfigOracle hpRel userConfig = do
    _ <- addOracle $
            \(HpReleaseQ _) -> return $ show hpRel
    _ <- addOracle $
            \(GhcBinDistTarFileQ _) -> return tarFile
    _ <- addOracle $
            \(CabalExeQ _) -> return cabalexe
    _ <- addOracle $
            \(StackExeQ _) -> return stackexe
    _ <- addOracle $
            \(GhcUsersGuidePDFFileQ _) -> return ghcUgPdf
    _ <- addOracle $
            \(GhcUsersGuideHTMLTarFileQ _) -> return ghcUgHtml
    _ <- addOracle $
            \(GhcLibsHTMLTarFileQ _) -> return ghcLibsHtml
    _ <- addOracle $
            \(HaddockHTMLTarFileQ _) -> return haddockHtml
    _ <- addOracle $
            \(BuildConfigQ _) -> either fail (return . show) buildConfig
    either fail return buildConfig
  where
    tarFile = ucGHCBinDist userConfig
    cabalexe = ucCabalExe userConfig
    stackexe = ucStackExe userConfig
    prefix = ucPrefix userConfig
    ghcUgPdf = ucGHCUsersPDF userConfig
    ghcUgHtml = ucGHCUsersHTML userConfig
    ghcLibsHtml = ucGHCLibsHTML userConfig
    haddockHtml = ucHaddockHTML userConfig
    includeExtra = ucBuildFlavor userConfig == BuildFlavorFull
    buildConfig = extractBuildConfig hpRel tarFile prefix includeExtra


extractBuildConfig :: Release -> FilePath -> Maybe FilePath -> Bool
                        -> Either String BuildConfig
extractBuildConfig hpRel tarFile prefix bcIncludeExtra =
    if ok then Right $ BuildConfig {..}
          else Left $ "extractBuildConfig tar file unparseable: " ++ base
  where
    -- example tarFile: ghc-7.8.0.20140228-x86_64-apple-darwin-lion.tar.bz2
    base0 = dropExtension $ takeFileName tarFile
    base = (if takeExtension base0 == ".tar" then dropExtension else id) base0
    parts = splitOn "-" base
    (ghcPrefix : verStr : bcArch : bcOsVendor : bcOs : remainder) = parts
    bcGhcVersion = GhcVersion $ version verStr
    bcOsDistribution = intercalate "-" remainder
    bcHpVersion = relVersion hpRel
    bcPrefix = prefix
    ok = (length parts >= 5) && (ghcPrefix == "ghc")
