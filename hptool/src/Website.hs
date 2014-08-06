{-# LANGUAGE RecordWildCards #-}

module Website where

import Data.Function (on)
import Data.List (groupBy)
import Development.Shake
import Text.Hastache
import Text.Hastache.Context

import Dirs
import Paths
import ReleaseFiles
import Templates

websiteRules :: FilePath -> Rules ()
websiteRules templateSite = do
    websiteDir */> \dst -> do
        bcCtx <- buildConfigContext
        let rlsCtx = releasesCtx releaseFiles
            ctx = ctxConcat [rlsCtx, bcCtx, errorCtx]
        copyExpandedDir ctx templateSite dst


fileCtx :: (Monad m) => FileInfo -> MuContext m
fileCtx (os, mArch, url, mHash) = mkStrContext ctx
  where
    ctx "osName" = MuVariable os
    ctx "osNameAndArch" = MuVariable $ os ++ maybe "" (\a -> ", " ++ a) mArch
    ctx "url" = MuVariable url
    ctx "mHash" = maybe (MuBool False) MuVariable mHash
    ctx _ = MuNothing

releaseCtx :: (Monad m) => ReleaseFiles -> MuContext m
releaseCtx (ver, (month, year), files) = mkStrContext ctx
  where
    ctx "version" = MuVariable ver
    ctx "year" = MuVariable $ show year
    ctx "month" = MuVariable $ monthName month
    ctx "files" = MuList $ map fileCtx files
    ctx _ = MuNothing

releasesCtx :: (Monad m) => [ReleaseFiles] -> MuContext m
releasesCtx allRs = mkStrContext ctx
  where
    ctx "years" = MuList $ map (mkStrContext . yearCtx) years
    ctx _ = MuNothing

    yearCtx [] _ = MuBool False
    yearCtx (r0:_) "year" = MuVariable $ show $ releaseYear r0
    yearCtx rs "releases" = MuList $ map releaseCtx rs
    yearCtx _ _ = MuNothing

    years = groupBy ((==) `on` releaseYear) allRs

releaseYear :: ReleaseFiles -> Int
releaseYear (_ver, (_month, year), _files) = year

monthName :: Int -> String
monthName i = maybe (show i) id $ lookup i monthNames
  where
    monthNames = zip [1..] $
        words "January Feburary March April May June \
              \July August September October November December"


