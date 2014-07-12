module HaddockMaster
    ( haddockDocDir
    , haddockMasterRules
    )
  where

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
        haddocMasterAction outdir hpRel bc'
        osDocAction (osFromConfig bc')


haddocMasterAction :: FilePath -> Release -> BuildConfig -> Action ()
haddocMasterAction outdir hpRel bc = do
    need $ vdir ghcVirtualTarget
           : map (dir . targetPkgDir) platformLibs

    ghcInterfaces <- mapM ghcInfo coreLibs
    hpInterfaces <- mapM hpInfo $ platformLibs
    localCommand' [] "haddock"
        (baseArgs ++ map readArg (ghcInterfaces ++ hpInterfaces))
  where
    baseArgs = [ "--odir=" ++ outdir
               , "--gen-index"
               , "--gen-contents"
               , "--title=\"Haskell Platform\""
               ]
    readArg (p,i) = "--read-interface=" ++ p ++ "," ++ i

    -- Is this build targetting a Windows platform?
    buildWin = (bcOs bc) == "mingw32"

    isForOS i = okForAll || (isWindows i && buildWin) ||
                (isNotWindows i && not buildWin)
        where okForAll = not (isWindows i) && not (isNotWindows i)

    coreLibs =
        packagesByIncludeFilter (\i -> isForOS i && isLib i && isGhc i) hpRel
    platformLibs =
        packagesByIncludeFilter (\i -> isForOS i && isLib i && not (isGhc i)) hpRel

    fieldHtml = "haddock-html"
    fieldIntf = "haddock-interfaces"

    os = osFromConfig bc

    targetGhcDb = targetDir </+> osGhcPrefix os </> ghcDb
    ghcDb = osGhcDbDir os

    targetPkgDir pkg = targetDir </+> osPackageTargetDir os pkg

    ghcInfo pkg = do
        p <- getFromGhcPkg fieldHtml pkg
                ["--package-db=" ++ targetGhcDb]
        i <- getFromGhcPkg fieldIntf pkg []
        return (p,i)

    hpInfo pkg = do
        p <- getFromConfFile fieldHtml $ packageTargetConf pkg
        i <- getFromConfFile fieldIntf $ packageInplaceConf pkg
        return (p,i)

    getFromGhcPkg field pkg extra = do
        Stdout out <- localCommand [] "ghc-pkg" $
                            extra ++ ["field", pkgName pkg, field ]
        extractField field out

    getFromConfFile field file = readFile' file >>= extractField field

    extractField field out = either error return . ex . lines $ out
      where
        ex [] = Left $ "Couldn't extract field " ++ field ++ " from:\n" ++ out
        ex (l:ls) = case words l of
            (k:vs) | k == fieldColon -> Right $ unwords vs
            _ -> ex ls

        fieldColon = field ++ ":"

