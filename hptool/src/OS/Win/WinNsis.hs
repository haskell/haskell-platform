{-# LANGUAGE RecordWildCards #-}

module OS.Win.WinNsis ( genNsisFiles ) where

import Control.Monad ( join )
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Version ( Version )
import Development.Shake
import Development.Shake.FilePath ( toNative, takeDirectory, takeFileName )
import System.FilePath ( equalFilePath, splitDirectories )
import System.Posix.Types ( FileOffset )
import System.PosixCompat.Files ( fileSize, getFileStatus )
import Text.Hastache ( MuType(..), MuContext )
import Text.Hastache.Context (mkStrContext)

import Config
import OS.Win.WinPaths
import OS.Win.WinUtils
import Paths ( phonyTargetDir )
import Templates ( copyExpandedFile, ctxAppend, platformContext )
import Types
import Utils


data InfoForTemplate = InfoForTemplate
    { iftGetProdName :: (Bool -> Version -> String -> FilePath)
    , iftNSISFile :: FilePath
    , iftStackFile :: FilePath
    , iftStackInstalledSize :: FileOffset
    , iftMainInstalledSize :: FileOffset
    }

-- | Using a template, generate the NSIS files.  Note that this file will
-- contain definitions of variables which are used by the inst.dat and
-- uninst.dat files (which get included into the NSIS file during the build
-- of the installer).  That is, there is a coupling between these files.
genNsisFiles :: Rules ()
genNsisFiles = do
    ghcNsisInstDat %> makeInstDat ghcInstFilter winTargetDir
    ghcNsisUninstDat %> makeUninstDat ghcUninstFilter winTargetDir

    msysNsisInstDat %> makeInstDat filterEmptyDirs winMSysTargetDir
    msysNsisUninstDat %> makeUninstDat sortByDirRev winMSysTargetDir

    extralibsNsisInstDat %> makeInstDat filterEmptyDirs winHpTargetDir
    extralibsNsisUninstDat %> makeUninstDat sortByDirRev winHpTargetDir

    nsisFile %> expandAndCopy nsiTemplate winProductFile
    ghcNsisFile %> expandAndCopy ghcNsiTemplate ghcProductFile
    msysNsisFile %> expandAndCopy msysNsiTemplate msysProductFile
    extralibsNsisFile %> expandAndCopy extralibsNsiTemplate extralibsProductFile

    -- Create the main doc/index.html from the template
    docIndexFile %> \_ -> do
        pCtx <- platformContext
        copyExpandedFile pCtx docIndexTmpl docIndexFile

  where
    makeInstDat instFilter targDir dFile = do
        need [phonyTargetDir]
        dirs <- getDirsFiles instFilter targDir
        genData nsisInstDatTmpl dFile dirs
    makeUninstDat uninstFilter targDir uFile = do
        need [phonyTargetDir]
        dirs <- getDirsFiles uninstFilter targDir
        genData nsisUninstDatTmpl uFile dirs

    -- ghc bucket covers all directories and files not in msys or extralibs
    -- since targetDir contains the msys sub-dir as well as lib/extralibs,
    -- we need to filter those paths out as they are covered by the
    -- msys and extralibs .dat lists.  N.b.: Filtering by names could be
    -- fragile in light of any directory, package, structuring changes.
    -- Also note that even in for the "core" HP build, there are still
    -- a few things in extralibs (specifically alex and happy, as well as
    -- the cabal executable.)
    ghcInstFilter = filterEmptyDirs . skipExtralibs . skipMSys
    ghcUninstFilter =  sortByDirRev . skipExtralibs . skipMSys

    -- For install, sort doesn't matter when parent vs child dir is created;
    -- For uninstall, deeper directories must be removed before their parent
    sortByDirRev = sortBy (flip (comparing fst))
        -- 'flip' comparing --> 'reverse'

    -- Filter directories with no files (but other directories are ok) for
    -- install but leave them for uninstall.
    filterEmptyDirs = filter (\(_,fs) -> not $ null fs)

    skipExtralibs = filter (\(d, _) ->
        not $ any (equalFilePath "extralibs") (splitDirectories d))

    skipMSys = filter (\(d, _) ->
        not $ any (equalFilePath "msys") (splitDirectories d))

    getDirsFiles f dir = liftIO $
        makeNativeRelPaths <$>
        f <$>
        getDirContentsR dir
      where
        makeNativeRelPath = toNative . (`relativeToDir` winTargetDir)
        makeNativeRelPaths =
            map (\(d,fs) -> ( makeNativeRelPath d, map makeNativeRelPath fs ))

    genData tmpl file dirs = do
        ctx <- mu <$> pure tmpl <*> pure dirs
        copyExpandedFile ctx tmpl file

    mu tmpl dirs = mkStrContext ex
      where
        ex "eachDir" = MuList $ map (mkStrContext . exDir) dirs
        ex t = error $ "GenNsis.ex: unexpected template tag " ++ t ++
               " while processing '" ++ tmpl ++ "'"

        exDir (dn,_) "dir" = MuVariable dn
        exDir (_,files) "eachFile" = MuList $ map (mkStrContext . exFile) files
        exDir _ t = error $ "GenNsis.exDir: unexpected template tag " ++ t ++
                    " while processing '" ++ tmpl ++ "'"

        exFile fn "file" = MuVariable fn
        exFile _ t = error $ "GenNsis.exFile: unexpected template tag " ++ t ++
                     " while processing '" ++ tmpl ++ "'"

    getFileSize :: FilePath -> IO FileOffset
    getFileSize f = fileSize <$> getFileStatus f
    getDirContentSize f dir = liftIO $
        (sum <$>) $
        join $ (mapM (\(_,fs) -> sum <$> mapM getFileSize fs)) <$>
        f <$>
        getDirContentsR dir

    expandAndCopy templ pFile nFile = do
        stackFile <- askStackExe
        bc <- askBuildConfig
        rls <- askHpRelease
        pCtx <- platformContext
        mainSize <- getDirContentSize filterEmptyDirs winTargetDir
        let nsisCtx = expandNsisInfo rls bc ift
            ctx = nsisCtx `ctxAppend` pCtx
            stackSize = 48 * 1024 * 1024 -- not sure how to automatically determine
            ift = InfoForTemplate pFile nFile stackFile stackSize mainSize
        copyExpandedFile ctx templ nFile

expandNsisInfo :: (Monad m) => Release -> BuildConfig -> InfoForTemplate ->
                  MuContext m
expandNsisInfo rls BuildConfig{..} InfoForTemplate{..} = mkStrContext ex
  where
    productFile = iftGetProdName bcIncludeExtra hpver bcArch
        `relativeToDir` takeDirectory iftNSISFile
        -- NSIS tool needs to run from the installerPartsDir

    ex "productFile" = MuVariable . toNative $ productFile
    ex "build64bit" = MuBool is64
    ex "is32or64" = MuVariable $ if is64 then "64" else "32"
    ex "programFiles64" = MuVariable $ if is64 then "64" else ""
    ex "targetFiles" = MuVariable . toNative $
        winTargetDir `relativeToDir` takeDirectory iftNSISFile
        -- NSIS is run from where the nsis is, so make relative to that
    ex "includeExtras" = MuBool bcIncludeExtra
    ex "mainInstalledSize" = MuVariable . show . inKB $ iftMainInstalledSize
    ex "stackInstallerPath" = MuVariable . toNative $
        iftStackFile `relativeToDir` takeDirectory iftNSISFile
        -- NSIS is run from where the nsis is, so make relative to that
    ex "stackInstallerFileName" = MuVariable . toNative $
        takeFileName iftStackFile
    ex "stackInstalledSize" = MuVariable . show . inKB $ iftStackInstalledSize
    ex _ = MuNothing

    is64 = bcArch == "x86_64"
    hpver = hpVersion . relVersion $ rls
    -- NSIS wants this in "kilobytes"
    inKB :: FileOffset -> Int
    inKB b = truncate $ (fromIntegral b) / (1024::Double)
