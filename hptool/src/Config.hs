{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             RecordWildCards, TypeFamilies #-}

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
import Utils (version)


newtype HpReleaseQ = HpReleaseQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult HpReleaseQ = Release

-- | Provide the Platform release information
-- The release information will be tracked as a dependency
askHpRelease :: Action Release
askHpRelease = askOracle $ HpReleaseQ ()


newtype GhcBinDistTarFileQ = GhcBinDistTarFileQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GhcBinDistTarFileQ = FilePath

-- | Provide the bindist tar file.
-- The filepath will be tracked as a dependency.
askGhcBinDistTarFile :: Action FilePath
askGhcBinDistTarFile = do
    tarFile <- askOracle $ GhcBinDistTarFileQ ()
    need [tarFile]
    return tarFile


newtype BuildConfigQ = BuildConfigQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult BuildConfigQ = BuildConfig

-- | Provide the Platform release information
-- The release information will be tracked as a dependency
askBuildConfig :: Action BuildConfig
askBuildConfig = askOracle $ BuildConfigQ ()


newtype StackExeQ = StackExeQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult StackExeQ = FilePath

-- | Provide the stack executable
-- The filepath will be tracked as a dependency
askStackExe :: Action FilePath
askStackExe = do
    stackexe <- askOracle $ StackExeQ ()
    need [stackexe]
    return stackexe


newtype CabalExeQ = CabalExeQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult CabalExeQ = FilePath

-- | Provide the stack executable
-- The filepath will be tracked as a dependency
askCabalExe :: Action FilePath
askCabalExe = do
    cabalexe <- askOracle $ CabalExeQ ()
    need [cabalexe]
    return cabalexe

newtype GhcUsersGuidePDFFileQ = GhcUsersGuidePDFFileQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GhcUsersGuidePDFFileQ = FilePath

askGhcUgPDF :: Action FilePath
askGhcUgPDF = do
  fname <- askOracle $ GhcUsersGuidePDFFileQ ()
  need [fname]
  return fname


newtype GhcUsersGuideHTMLTarFileQ = GhcUsersGuideHTMLTarFileQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GhcUsersGuideHTMLTarFileQ = FilePath

askGhcUgHtml :: Action FilePath
askGhcUgHtml = do
  fname <- askOracle $ GhcUsersGuideHTMLTarFileQ ()
  need [fname]
  return fname


newtype GhcLibsHTMLTarFileQ =  GhcLibsHTMLTarFileQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GhcLibsHTMLTarFileQ = FilePath

askGhcLibs :: Action FilePath
askGhcLibs = do
  fname <- askOracle $ GhcLibsHTMLTarFileQ ()
  need [fname]
  return fname


newtype HaddockHTMLTarFileQ = HaddockHTMLTarFileQ ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult HaddockHTMLTarFileQ = FilePath

askHaddockHTML :: Action FilePath
askHaddockHTML = do
  fname <- askOracle $ HaddockHTMLTarFileQ ()
  need [fname]
  return fname


addConfigOracle :: Release -> UserConfig -> Rules BuildConfig
addConfigOracle hpRel userConfig = do
    _ <- addOracle $
            \(HpReleaseQ _) -> return hpRel
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
            \(BuildConfigQ _) -> either fail return buildConfig
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
