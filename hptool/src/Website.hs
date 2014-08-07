{-# LANGUAGE RecordWildCards #-}

module Website where

import Data.Char (toLower)
import Data.Function (on)
import Data.List (find, groupBy, nub, sort)
import Data.Version (showVersion, versionBranch)
import Development.Shake
import Text.Hastache
import Text.Hastache.Context

import Dirs
import Paths
import PlatformDB
import Releases
import ReleaseFiles
import Templates
import Types

websiteRules :: FilePath -> Rules ()
websiteRules templateSite = do
    websiteDir */> \dst -> do
        bcCtx <- buildConfigContext
        let rlsCtx = releasesCtx releaseFiles
            ctx = ctxConcat [rlsCtx, historyCtx, bcCtx, errorCtx]
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

historyCtx :: (Monad m) => MuContext m
historyCtx = mkStrContext outerCtx
  where
    outerCtx "history" = MuList [mkStrContext ctx]
    outerCtx _ = MuNothing

    ctx "hpReleases" = MuList $ map (mkStrContext . rlsCtx) releasesNewToOld
    ctx "ncols" = MuVariable $ length releasesNewToOld + 1
    ctx "sections" = MuList
        [ sectionCtx "Compiler"                            [isGhc, not . isLib]
        , sectionCtx "Core Libraries, Provided with GHC"   [isGhc, isLib]
        , sectionCtx "Additional Platform Libraries"       [not . isGhc, isLib]
        , sectionCtx "Programs and Tools"                  [isTool]
        ]
    ctx _ = MuNothing

    rlsCtx rls "hpVersion" = MuVariable . showVersion . hpVersion . relVersion $ rls
    rlsCtx _ _ = MuNothing

sectionCtx :: (Monad m) => String -> [IncludeType -> Bool] -> MuContext m
sectionCtx name tests = mkStrContext ctx
  where
    ctx "name" = MuVariable name
    ctx "components" = MuList $ map (mkStrContext . pCtx) packages
    ctx _ = MuNothing

    packages = sortOnLower . nub . map pkgName . concat $
                map (packagesByIncludeFilter $ \i -> all ($i) tests)
                 releasesNewToOld

    sortOnLower = map snd . sort . map (\s -> (map toLower s, s))

    pCtx pName "package" = MuVariable pName
    pCtx pName "hackageUrl" =
        MuVariable $ "http://hackage.haskell.org/package/" ++ pName
    pCtx pName "releases" =
        MuList $ map (mkStrContext . pvCtx) $ packageVersionInfo pName
    pCtx _ _ = MuNothing

    pvCtx (c, _) "class" = MuVariable c
    pvCtx (_, v) "version" = MuVariable v
    pvCtx _ _ = MuNothing


packageVersionInfo :: String -> [(String, String)]
packageVersionInfo pName = curr $ zipWith comp vers (drop 1 vers ++ [Nothing])
  where
    comp Nothing  _                         = ("missing", "—")
    comp (Just v) Nothing                   = ("update", showVersion v)
    comp (Just v) (Just w) | maj v == maj w = ("same",   showVersion v)
                           | otherwise      = ("update", showVersion v)

    maj = take 3 . versionBranch

    curr ((c, v) : cvs) = (c ++ " current", v) : cvs
    curr [] = []

    vers = map (fmap pkgVersion . find ((==pName) . pkgName) . map snd . relIncludes)
            releasesNewToOld

releasesNewToOld :: [Release]
releasesNewToOld = reverse releases
