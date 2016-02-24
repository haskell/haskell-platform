{-# LANGUAGE PatternGuards, RecordWildCards #-}

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
    websiteDir %/> \dst -> do
        bcCtx <- buildConfigContext
        let rlsCtx = releasesCtx
            ctx = ctxConcat [rlsCtx, historyCtx, bcCtx, currentPlatformCtx, errorCtx]
        copyExpandedDir ctx templateSite dst

currentPlatformCtx :: Monad m => MuContext m
currentPlatformCtx = mkStrContext ctx
    where
      ctx "freezeConfig" = mapListStrContext go freezeIncludes
      ctx _ = MuNothing

      go x "name" = MuVariable $ pkgName x
      go x "version" = MuVariable . showVersion $ pkgVersion x
      go _ _ = MuNothing

      freezeIncludes = map snd . filter filt . allRelIncludes $ head (reverse releases)
      filt x = not (fst x `elem` [IncGHC, IncGHCLib, IncGHCTool, IncTool])


fileCtx :: (Monad m) => FileInfo -> MuContext m
fileCtx (dist, url, mHash, isFull) = mkStrContext ctx
  where
    ctx "osNameAndArch" = MuVariable $ distName dist
    ctx "url" = MuVariable url
    ctx "mHash" = maybe (MuBool False) MuVariable mHash
    ctx "archBits"
      | DistBinary _ arch <- dist = MuVariable $ archBits arch
      | otherwise = MuNothing

    ctx "isOSX"     = MuBool $ distIsFor OsOSX     dist
    ctx "isWindows" = MuBool $ distIsFor OsWindows dist
    ctx "isLinux"   = MuBool $ distIsFor OsLinux   dist
    ctx "isSource"  = MuBool $ dist == DistSource
    ctx "isFull"    = MuBool $ isFull

    ctx _ = MuNothing

releaseCtx :: (Monad m) => ReleaseFiles -> MuContext m
releaseCtx (ver, (month, year), files) = mkStrContext ctx
  where
    ctx "version" = MuVariable ver
    ctx "year" = MuVariable $ show year
    ctx "month" = MuVariable $ monthName month
    ctx "files" = mapListContext fileCtx files
    ctx _ = MuNothing

releasesCtx :: (Monad m) => MuContext m
releasesCtx = mkStrContext ctx
  where
    ctx "years" = mapListStrContext yearCtx years
    ctx "current" = MuList [releaseCtx currentFiles]
    ctx _ = MuNothing

    yearCtx [] _ = MuBool False
    yearCtx (r0:_) "year" = MuVariable $ show $ releaseYear r0
    yearCtx rs "releases" = mapListContext releaseCtx rs
    yearCtx _ _ = MuNothing

    years = groupBy ((==) `on` releaseYear) priorFiles


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

    ctx "hpReleases" = mapListStrContext rlsCtx releasesNewToOld
    ctx "ncols" = MuVariable $ length releasesNewToOld + 1
    ctx "sections" = MuList
        [ sectionCtx "Compiler"                              [isGhc, not . isLib]
        , sectionCtx "Core Libraries, provided with GHC"     [isGhc, isLib]
        , sectionCtx "Additional Minimal Platform Libraries" [not . isGhc, isLib]
        , sectionCtx "Programs and Tools"                    [isTool]
        , extendedCtx "Libraries with Full Platform"
        ]
    ctx _ = MuNothing

    rlsCtx rls "hpVersion" = MuVariable . showVersion . hpVersion . relVersion $ rls
    rlsCtx _ _ = MuNothing

sectionCtx :: (Monad m) => String -> [IncludeType -> Bool] -> MuContext m
sectionCtx name tests = mkStrContext ctx
  where
    ctx "name" = MuVariable name
    ctx "components" = mapListStrContext pCtx packages
    ctx _ = MuNothing

    packages = sortOnLower . nub . map pkgName . concat $
                map (packagesByIncludeFilter (\i -> all ($i) tests) False)
                releasesNewToOld

    sortOnLower = map snd . sort . map (\s -> (map toLower s, s))

    pCtx pName "package" = MuVariable pName
    pCtx pName "hackageUrl" =
        MuVariable $ "http://hackage.haskell.org/package/" ++ pName
    pCtx pName "releases" = mapListStrContext pvCtx $ packageVersionInfo False pName
    pCtx _ _ = MuNothing

    pvCtx (c, _) "class" = MuVariable c
    pvCtx (_, v) "version" = MuVariable v
    pvCtx _ _ = MuNothing


extendedCtx :: (Monad m) => String -> MuContext m
extendedCtx name = mkStrContext ctx
  where
    ctx "name" = MuVariable name
    ctx "components" = mapListStrContext pCtx packages
    ctx _ = MuNothing

    packages = sortOnLower . nub . map pkgName . concat $
                map (map snd . relIncludes)
                releasesNewToOld

    sortOnLower = map snd . sort . map (\s -> (map toLower s, s))

    pCtx pName "package" = MuVariable pName
    pCtx pName "hackageUrl" =
        MuVariable $ "http://hackage.haskell.org/package/" ++ pName
    pCtx pName "releases" = mapListStrContext pvCtx $ packageVersionInfo True pName
    pCtx _ _ = MuNothing

    pvCtx (c, _) "class" = MuVariable c
    pvCtx (_, v) "version" = MuVariable v
    pvCtx _ _ = MuNothing

packageVersionInfo :: Bool -> String -> [(String, String)]
packageVersionInfo searchFull pName = curr $ zipWith comp vers (drop 1 vers ++ [Nothing])
  where
    comp Nothing  _                         = ("missing", "-")
    comp (Just v) Nothing                   = ("update", showVersion v)
    comp (Just v) (Just w) | maj v == maj w = ("same",   showVersion v)
                           | otherwise      = ("update", showVersion v)

    maj = take 3 . versionBranch

    curr ((c, v) : cvs) = (c ++ " current", v) : cvs
    curr [] = []

    vers = map (fmap pkgVersion . find ((==pName) . pkgName) . map snd . (if searchFull then allRelIncludes else relMinimalIncludes))
            releasesNewToOld

releasesNewToOld :: [Release]
releasesNewToOld = reverse releases
