{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forM_)
import Data.Monoid (mconcat)
import Development.Shake
import Development.Shake.FilePath
import System.Console.GetOpt
import qualified System.Info (os, arch)
import System.IO

import Config
import Dirs
import GhcDist
import HaddockMaster
import OS
import Package
import Paths
import PlatformDB
import Releases
import SourceTarball
import Types
import Target
import Website

data Flags = Info | Prefix String | Full
  deriving Eq

flags :: [OptDescr (Either a Flags)]
flags = [ Option ['i'] ["info"] (NoArg $ Right Info)
                     "Show info on what gets included in this HP release"
        , Option [] ["prefix"] (ReqArg (Right . Prefix) "DIR")
                     "Set installation prefix (only for Posix builds)"
        , Option ['f'] ["full"] (NoArg $ Right Full)
                     "Do a full (rather than minimal) build of the platform."
        ]

main :: IO ()
main = hSetEncoding stdout utf8 >> shakeArgsWith opts flags main'
  where
    main' flgs args =
        if Info `elem` flgs
            then info
            else case args of
                (tarfile:cabalexe:stackexe:buildType) -> return $ Just $ do
                    allRules tarfile (cabalexe,stackexe) flgs
                    want $ if null buildType then ["build-all"] else buildType
                _ -> usage

    info = do
        putStrLn $ "This hptool is built to construct " ++ hpFullName ++ "\n\
                   \  for the " ++
                   System.Info.os ++ " " ++ System.Info.arch ++ " platform.\n\
                   \The HP constructed will contain the following:\n\n"
                   ++ unlines (whatIsIncluded hpRelease)
        return Nothing

    usage = do
        putStrLn "usage: hptool --info\n\
                 \       hptool [opts] <ghc-bindist.tar.bz> <cabal executable> <stack executable> [target...]\n\
                 \  where target is one of:\n\
                 \    build-all           -- build everything (default)\n\
                 \    build-source        -- build the source tar ball\n\
                 \    build-target        -- build the target tree\n\
                 \    build-product       -- build the os specific installer\n\
                 \    build-package-<pkg> -- build the package (name or name-ver)\n\
                 \    build-local         -- build the local GHC environment\n\
                 \    build-website       -- build the website\n\
                 \  and opts may be 'f' for a full rather than minimal build, 'i' for info\n\
                 \  or 'prefix=...' to set a custom install location prefix for linux"
        return Nothing

    allRules tarfile stackcabalexe flgs = do
        buildConfig <- addConfigOracle hpRelease tarfile stackcabalexe (prefixSetting flgs) (Full `elem` flgs)
        ghcDistRules
        packageRules
        targetRules buildConfig
        haddockMasterRules buildConfig
        sourceTarballRules srcTarFile
        buildRules hpRelease srcTarFile buildConfig
        websiteRules "website"

    prefixSetting = mconcat . reverse . map ps
      where
        ps (Prefix p) = Just p
        ps _ = Nothing

    opts = shakeOptions

    hpRelease = hp_8_0_1
    hpFullName = show $ relVersion hpRelease
    srcTarFile = productDir </> hpFullName <.> "tar.gz"


whatIsIncluded :: Release -> [String]
whatIsIncluded rel = ("-- Minimal Platform:":minimalIncludes) ++ ("-- Full Platform:":fullIncludes) where
    minimalIncludes = map (concat . includeToString) $ relMinimalIncludes rel
    fullIncludes    = map (concat . includeToString) $ relIncludes rel where
    includeToString (IncGHC, p)      = "GHC:    " : [show p]
    includeToString (IncGHCLib, p)   = "GHCLib: " : [show p]
    includeToString (IncLib, p)      = "LIB:    " : [show p]
    includeToString (IncTool, p)     = "TOOL:   " : [show p]
    includeToString (IncGHCTool, p)  = "TOOL:   " : [show p]
    includeToString (IncIfWindows it, p) =
                                       "IfWindows:    " : includeToString (it,p)
    includeToString (IncIfNotWindows it, p) =
                                       "IfNotWindows: " : includeToString (it,p)

buildRules :: Release -> FilePath -> BuildConfig -> Rules()
buildRules hpRelease srcTarFile bc = do
    "build-source" ~> need [srcTarFile]
    "build-target" ~> need [targetDir]
    "build-product" ~> need [osProduct]
    "build-local" ~> need [dir ghcLocalDir]
    "build-website" ~> need [dir websiteDir]
    forM_ (platformPackages True hpRelease) $ \pkg -> do
        let full = "build-package-" ++ show pkg
        let short = "build-package-" ++ pkgName pkg
        short ~> need [full]
        full ~> need [dir $ packageBuildDir pkg]
    "build-all" ~> do  -- separate need call so built in order
        need ["build-source"]
        need ["build-product"]
    osRules hpRelease bc
  where
    OS{..} = osFromConfig bc
