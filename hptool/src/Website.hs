{-# LANGUAGE RecordWildCards #-}

module Website where

import Control.Monad (forM_, when)
import qualified Data.Text.Lazy.IO as TL
import Data.Version (showVersion)
import Development.Shake
import Development.Shake.FilePath
import Text.Hastache
import Text.Hastache.Context

import Config
import Dirs
import Paths
import Types
import Utils

websiteRules :: FilePath -> Rules ()
websiteRules templateSite = do
    websiteDir */> \dst -> do
        bc <- askBuildConfig
        websiteAction templateSite bc templateSite dst

websiteAction :: FilePath -> BuildConfig -> FilePath -> FilePath -> Action ()
websiteAction templateSite BuildConfig{..} = copy
  where
    copy src dst = do
        putLoud $ "processing " ++ src ++ " to " ++ dst
        makeDirectory dst

        srcFiles <- getDirectoryFiles src ["*"]
        forM_ srcFiles $ \f -> do
            putLoud $ "  file " ++ src </> f
            (fileAction f) (src </> f) (dst </> f)

        srcDirs <- getDirectoryDirs src
        forM_ srcDirs $ \d -> do
            putLoud $ "  dir " ++ src </> d
            copy (src </> d) (dst </> d)

    fileAction f = case takeExtension f of
        ".mu" -> expandFile
        _ -> copyFile'

    expandFile from to = let to' = dropExtension to in
        when (not $ null $ takeExtension to') $ do
            need [from]
            hastacheFile muConf from muCtx >>= liftIO . TL.writeFile to'

    muConf = (defaultConfig :: MuConfig Action)
                { muTemplateFileDir = Just templateSite
                , muTemplateFileExt = Just ".mu"
                , muTemplateRead =
                    \f -> need [f] >> muTemplateRead defaultConfig f
                }

    muCtx = mkStrContext ctx

    ctx "hpVersion" = MuVariable . showVersion . hpVersion $ bcHpVersion
    ctx t = error $ "ctx: unexpected template tag " ++ t

