{-# LANGUAGE RecordWildCards #-}

module HaddockMaster
    ( haddockDocDir
    , haddockMasterRules
    , haddockAllCorePkgLocs
    , haddockPlatformPkgLocs
    , haddockReadArg
    )
  where

import Control.Applicative ((<$>))
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
    haddockDocDir bc %/> \outdir -> do
        hpRel <- askHpRelease
        bc' <- askBuildConfig
        haddockMasterAction outdir hpRel bc'
        osDocAction (osFromConfig bc')


haddockMasterAction :: FilePath -> Release -> BuildConfig -> Action ()
haddockMasterAction outdir hpRel bc = do
    need $ vdir ghcVirtualTarget
           : map (dir . targetPkgDir) (allPlatformLibs hpRel bc)
    cReadArgs <- map (haddockReadArg . osGhcPkgPathMunge outdir)
                 <$> haddockAllCorePkgLocs hpRel bc
    pReadArgs <- map (haddockReadArg . osPlatformPkgPathMunge outdir)
                 <$> haddockAllPlatformPkgLocs hpRel bc
    localCommand' [] "haddock" (baseArgs ++ cReadArgs ++ pReadArgs)
  where
    baseArgs = [ "--odir=" ++ outdir
               , "--gen-index"
               , "--gen-contents"
               , "--title=\"Haskell Platform\""
               ]

    OS{..} = osFromConfig bc

    targetPkgDir pkg = targetDir </+> osPackageTargetDir pkg


haddockAllCorePkgLocs :: Release -> BuildConfig -> Action [HaddockPkgLoc]
haddockAllCorePkgLocs hpRel bc = do
    mapM (ghcInfo os) $ allCoreLibs hpRel bc
  where
    os = osFromConfig bc

haddockAllPlatformPkgLocs :: Release -> BuildConfig -> Action [HaddockPkgLoc]
haddockAllPlatformPkgLocs hpRel bc =
    haddockPlatformPkgLocs $ allPlatformLibs hpRel bc

haddockPlatformPkgLocs :: [Package] -> Action [HaddockPkgLoc]
haddockPlatformPkgLocs = mapM (hpInfo)


isForOS :: String -> IncludeType -> Bool
isForOS os i = okForAll
            || (isWindows i && buildWin)
            || (isNotWindows i && not buildWin)
  where
    okForAll = not (isWindows i) && not (isNotWindows i)
    buildWin = os == "mingw32" -- Is this build targetting a Windows platform?

allCoreLibs :: Release -> BuildConfig -> [Package]
allCoreLibs hpRel bc = packagesByIncludeFilter
    (\i -> isForOS (bcOs bc) i && isLib i && isGhc i) (bcIncludeExtra bc) hpRel

allPlatformLibs :: Release -> BuildConfig -> [Package]
allPlatformLibs hpRel bc = packagesByIncludeFilter
    (\i -> isForOS (bcOs bc) i && isLib i && not (isGhc i)) (bcIncludeExtra bc) hpRel


haddockReadArg :: HaddockPkgLoc -> String
haddockReadArg HaddockPkgLoc{..} =
    "--read-interface=" ++ pkgLocHtml ++ "," ++ pkgLocIntf


ghcInfo :: OS -> Package -> Action HaddockPkgLoc
ghcInfo OS{..} pkg = do
    p <- getFromGhcPkg fieldHtml $
               ["--package-db=" ++ targetGhcDb] ++ osGhcPkgHtmlFieldExtras
    i <- getFromGhcPkg fieldIntf []
    return $ HaddockPkgLoc p i
  where
    getFromGhcPkg field extra = do
        Stdout out <- localCommand [] "ghc-pkg" $
                            extra ++ ["field", pkgName pkg, field ]
        either error return $ extractField field out

    targetGhcDb = targetDir </+> osGhcPrefix </> osGhcDbDir

hpInfo :: Package -> Action HaddockPkgLoc
hpInfo pkg = do
    p <- getFromConfFile fieldHtml $ packageTargetConf pkg
    i <- getFromConfFile fieldIntf $ packageInplaceConf pkg
    return $ HaddockPkgLoc p i
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
