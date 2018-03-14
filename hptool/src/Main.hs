{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad ( forM_ )
import Data.List ( intercalate )
import Development.Shake
import Development.Shake.FilePath
import System.Console.GetOpt ( usageInfo )
import System.Directory as SD ( doesFileExist )
import qualified System.Info (os, arch)
import System.IO

import CLArgs
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


main :: IO ()
main = hSetEncoding stdout utf8 >> shakeArgsWith opts flags main'
  where
    main' flgVals nonFlagArgs =
      if Info `elem` flgVals then info
      else
        if Usage `elem` flgVals then usage
        else do
          -- if we were to do cross-building, using System.Info.os is incorrect
          let buildWin = System.Info.os == "mingw32"
          (uc, v) <- checkAndValidateArgs buildWin SD.doesFileExist flgVals
          if (not $ null v )
            then do
              putStrLn "ERRORS:"
              putStrLn $ intercalate "\n" v
              usage
            else return $ Just $ do
              allRules uc
              want $ if null nonFlagArgs then ["build-all"] else nonFlagArgs

    info = do
        putStrLn $ "This hptool is built to construct " ++ hpFullName ++ "\n\
                   \  for the " ++
                   System.Info.os ++ " " ++ System.Info.arch ++ " platform.\n\
                   \The HP constructed will contain the following:\n\n"
                   ++ unlines (whatIsIncluded hpRelease)
        return Nothing

    usage = do
        putStr   $ usageInfo
                 "usage: hptool --info\n\
                 \       hptool --help\n\
                 \       hptool [args] [target...]\n\
                 \  where target is one of:\n\
                 \    build-all           -- build everything (default)\n\
                 \    build-source        -- build the source tar ball\n\
                 \    build-target        -- build the target tree\n\
                 \    build-product       -- build the os specific installer\n\
                 \    build-package-<pkg> -- build the package (name or name-ver)\n\
                 \    build-local         -- build the local GHC environment\n\
                 \    build-website       -- build the website\n\
                 \  and args may be:" flags
        putStrLn "NOTE:\n\
                 \     The GHC binary distro, the cabal executable, and the \n\
                 \     stack installer/executable are *required*.\n\
                 \     Further, for the Windows platform, four additional\n\
                 \     arguments are also required:\n\
                 \         * the GHC Library documentation (tarfile)\n\
                 \         * the GHC User's Guide (HTML format, tarfile)\n\
                 \         * the GHC User's Guide (single PDF file)\n\
                 \         * the Haddock documentation (HTML format, tarfile)"

        return Nothing

    allRules :: UserConfig -> Rules ()
    allRules userConfig = do
        buildConfig <- addConfigOracle hpRelease userConfig
        ghcDistRules
        packageRules
        targetRules buildConfig
        haddockMasterRules buildConfig
        sourceTarballRules srcTarFile
        buildRules hpRelease srcTarFile buildConfig
        websiteRules "website"

    opts = shakeOptions

    hpRelease = hp_8_4_3
    hpFullName = show $ relVersion hpRelease
    srcTarFile = productDir </> hpFullName <.> "tar.gz"


whatIsIncluded :: Release -> [String]
whatIsIncluded rel = ("-- Core Platform:":minimalIncludes) ++ ("-- Full Platform:":fullIncludes) where
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
    "build-target" ~> need [phonyTargetDir]
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
