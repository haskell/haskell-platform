{-# LANGUAGE RecordWildCards #-}

module HaddockMaster
    ( haddockDocDir
    , haddockMasterRules
    , haddockAllCoreReadArgs
    , haddockPlatformReadArgs
    )
  where

import Control.Applicative ( liftA )
import Development.Shake
import Development.Shake.FilePath

import Config
import Dirs
import LocalCommand
import OS
import Paths
import PlatformDB
import Types
import Utils


haddockDocDir :: BuildConfig -> FilePath
haddockDocDir bc = targetDir </+> osHpPrefix (osFromConfig bc) </> "doc"


haddockMasterRules :: BuildConfig -> Rules ()
haddockMasterRules bc =
    haddockDocDir bc */> \outdir -> do
        hpRel <- askHpRelease
        bc' <- askBuildConfig
        haddockMasterAction outdir hpRel bc'
        osDocAction (osFromConfig bc')


haddockMasterAction :: FilePath -> Release -> BuildConfig -> Action ()
haddockMasterAction outdir hpRel bc = do
    need $ vdir ghcVirtualTarget
           : map (dir . targetPkgDir) (allPlatformLibs hpRel bc)
    cReadArgs <- haddockAllCoreReadArgs outdir hpRel bc
    pReadArgs <- haddockAllPlatformReadArgs outdir hpRel bc
    localCommand' [] "haddock" (baseArgs ++ cReadArgs ++ pReadArgs)
  where
    baseArgs = [ "--odir=" ++ outdir
               , "--gen-index"
               , "--gen-contents"
               , "--title=\"Haskell Platform\""
               ]

    targetPkgDir pkg = targetDir </+> osPackageTargetDir (osFromConfig bc) pkg


haddockAllCoreReadArgs :: FilePath -> Release -> BuildConfig -> Action [String]
haddockAllCoreReadArgs baseDir hpRel bc = do
    mapM (ghcInfo baseDir os) $ allCoreLibs hpRel bc
  where
    os = osFromConfig bc

haddockAllPlatformReadArgs :: FilePath -> Release -> BuildConfig
                           -> Action [String]
haddockAllPlatformReadArgs baseDir hpRel bc =
    haddockPlatformReadArgs baseDir bc $ allPlatformLibs hpRel bc

haddockPlatformReadArgs :: FilePath -> BuildConfig -> [Package]
                        -> Action [String]
haddockPlatformReadArgs baseDir bc = mapM (hpInfo baseDir os)
  where
    os = osFromConfig bc



isForOS :: String -> IncludeType -> Bool
isForOS os i = okForAll
            || (isWindows i && buildWin)
            || (isNotWindows i && not buildWin)
  where
    okForAll = not (isWindows i) && not (isNotWindows i)
    buildWin = os == "mingw32" -- Is this build targetting a Windows platform?

allCoreLibs :: Release -> BuildConfig -> [Package]
allCoreLibs hpRel bc =
    packagesByIncludeFilter (\i -> isForOS (bcOs bc) i && isLib i && isGhc i) hpRel

allPlatformLibs :: Release -> BuildConfig -> [Package]
allPlatformLibs hpRel bc =
    packagesByIncludeFilter (\i -> isForOS (bcOs bc) i && isLib i && not (isGhc i)) hpRel


readArg :: (String, String) -> String
readArg (p,i) = "--read-interface=" ++ p ++ "," ++ i


ghcInfo :: FilePath -> OS -> Package -> Action String
ghcInfo baseDir OS{..} pkg = do
    p <- liftA (osGhcPkgPathMunge baseDir)
         . getFromGhcPkg fieldHtml $
               ["--package-db=" ++ targetGhcDb] ++ osGhcPkgHtmlFieldExtras
    i <- getFromGhcPkg fieldIntf []
    return $ readArg (p,i)
  where
    getFromGhcPkg field extra = do
        Stdout out <- localCommand [] "ghc-pkg" $
                            extra ++ ["field", pkgName pkg, field ]
        either error return $ extractField field out

    targetGhcDb = targetDir </+> osGhcPrefix </> osGhcDbDir

hpInfo :: FilePath -> OS -> Package -> Action String
hpInfo baseDir OS{..} pkg = do
    p <- liftA (osPlatformPkgPathMunge baseDir)
         . getFromConfFile fieldHtml $ packageTargetConf pkg
    i <- getFromConfFile fieldIntf $ packageInplaceConf pkg
    return $ readArg (p,i)
  where
    getFromConfFile field file =
        readFile' file >>= either error return . extractField field


fieldHtml, fieldIntf :: String
fieldHtml = "haddock-html"
fieldIntf = "haddock-interfaces"

extractField :: String -> String -> Either String String
extractField field out =  ex . lines $ out
  where
    ex [] = Left $ "Couldn't extract field " ++ field ++ " from:\n" ++ out
    ex (l:ls) = case words l of
        (k:vs) | k == fieldColon -> Right $ unwords vs
        _ -> ex ls

    fieldColon = field ++ ":"

