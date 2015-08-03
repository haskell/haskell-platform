{-# LANGUAGE RecordWildCards #-}

module OS.Win.WinNsis ( genNsisData, genNsisFile ) where

import Control.Applicative ( (<$>), (<*>), pure )
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Development.Shake
import Development.Shake.FilePath ( toNative, takeDirectory )
import Text.Hastache ( MuType(..), MuContext )
import Text.Hastache.Context (mkStrContext)

import Config
import OS.Win.WinPaths
import OS.Win.WinUtils
import Paths ( installerPartsDir, phonyTargetDir )
import Templates
import Types
import Utils


genNsisData :: Rules ()
genNsisData = do
    nsisInstDat %> \dFile -> do
        need [phonyTargetDir]
        dirs <- getDirsFiles filterEmptyDirs
        genData nsisInstDatTmpl dFile dirs
    nsisUninstDat %> \uFile -> do
        need [phonyTargetDir]
        dirs <- getDirsFiles sortByDirRev
        genData nsisUninstDatTmpl uFile dirs
  where
    -- For install, sort doesn't matter when parent vs child dir is created;
    -- For uninstall, deeper directories must be removed before their parent
    sortByDirRev = sortBy (flip (comparing fst)) -- 'flip' comparing --> 'reverse'

    -- Filter directories with no files (but other directories are ok) for
    -- install but leave them for uninstall.
    filterEmptyDirs = filter (\(_,fs) -> not $ null fs)

    getDirsFiles f = liftIO $
                     makeNativeRelPaths <$>
                     f <$>
                     getDirContentsR winTargetDir
      where
        makeNativeRelPaths =
            map (\(d,fs) -> ( toNative $ d ® winTargetDir
                            , map (toNative . (® winTargetDir)) fs))

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

-- | Using a template, generate the NSIS file.  Note that this file will
-- contain definitions of variables which are used by the inst.dat and
-- uninst.dat files (which get included into the NSIS file during the build
-- of the installer).  That is, there is a coupling between these files.
genNsisFile :: Rules ()
genNsisFile =
    nsisFile %> \nFile -> do
        bc <- askBuildConfig
        rls <- askHpRelease
        pCtx <- platformContext
        let nsisCtx = expandNsisInfo rls bc
            ctx = nsisCtx `ctxAppend` pCtx
        copyExpandedFile ctx nsiTemplate nFile

expandNsisInfo :: (Monad m) => Release -> BuildConfig -> MuContext m
expandNsisInfo rls BuildConfig{..} = mkStrContext ex
  where
    ex "productFile" = MuVariable . toNative $
        winProductFile hpver bcArch ® installerPartsDir
        -- NSIS tool needs to run from the installerPartsDir
    ex "build64bit" = MuBool is64
    ex "is32or64" = MuVariable $ if is64 then "64" else "32"
    ex "programFiles64" = MuVariable $ if is64 then "64" else ""
    ex "targetFiles" = MuVariable . toNative $
        winTargetDir ® takeDirectory nsisFile
        -- NSIS is run from where nsisFile is, so make relative to that

    ex _ = MuNothing

    is64 = bcArch == "x86_64"
    hpver = hpVersion . relVersion $ rls
