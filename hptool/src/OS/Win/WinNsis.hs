{-# LANGUAGE RecordWildCards #-}

module OS.Win.WinNsis ( genNsisFiles ) where

import Control.Monad ( join, void )
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Development.Shake
import Development.Shake.FilePath ( (</>), toNative, takeDirectory
                                  , takeFileName )
import System.FilePath ( equalFilePath, splitDirectories )
import System.Posix.Types ( FileOffset )
import System.PosixCompat.Files ( fileSize, getFileStatus )
import Text.Hastache ( MuType(..), MuContext )
import Text.Hastache.Context (mkStrContext)

import Config
import Dirs ( (%/>), dir, vdir )
import LocalCommand (localCommand' )
import OS.Win.WinPaths
import OS.Win.WinUtils
import Paths ( ghcLocalDir, ghcVirtualTarget, installerPartsDir, phonyTargetDir
             , targetDir )
import Templates ( copyExpandedFile, ctxAppend, platformContext )
import Types
import Utils


data InfoForTemplate = InfoForTemplate
    { iftProdName :: String    -- Name for (sub-)installer (e.g., "Foo")
    , iftProdFile :: FilePath  -- (Sub-)installer filename (e.g., "foo.exe")
    , iftNSISFile :: FilePath  -- (Sub-)installer.nsi path (rooted at buildRoot)
    , iftStackFile :: FilePath -- path to the stack installer
    , iftStackInstalledSize :: FileOffset -- size in bytes of stack (installed)
    , iftMainInstalledSize :: FileOffset  -- size in bytes of 'target' dir
    }

-- | Using a template, generate the NSIS files.  Note that this file will
-- contain definitions of variables which are used by the inst.dat and
-- uninst.dat files (which get included into the NSIS file during the build
-- of the installer).  That is, there is a coupling between these files.
genNsisFiles :: FilePath -> Rules ()
genNsisFiles osProduct = do
    ghcNsisInstDat %> makeInstDat ghcInstFilter winTargetDir
    ghcNsisUninstDat %> makeUninstDat ghcUninstFilter winTargetDir

    ghcDocNsisInstDat %> makeInstDat filterEmptyDirs winDocTargetDir
    ghcDocNsisUninstDat %> makeUninstDat sortByDirRev winDocTargetDir

    -- copy msys(msys2) pieces
    winMSysTargetDir %/> \_ -> do
        bc <- askBuildConfig
        copyDirAction (winExternalMSysDir bc) winMSysTargetDir

    msysNsisInstDat %> \fn -> do
        need [ dir winMSysTargetDir ]
        makeInstDat filterEmptyDirs winMSysTargetDir fn
    msysNsisUninstDat %> \fn -> do
        need [ dir winMSysTargetDir ]
        makeUninstDat sortByDirRev winMSysTargetDir fn

    extralibsNsisInstDat %> makeInstDat filterEmptyDirs winHpTargetDir
    extralibsNsisUninstDat %> makeUninstDat sortByDirRev winHpTargetDir

    commonNshFile %> \fn -> do
        need [ commonNshTemplate ]
        expandAndCopy "CommonNsh" commonNshTemplate commonNshFile fn
    nsisFile %> \fn -> do
        need [ nsiTemplate, commonNshFile ]
        expandAndCopy "Haskell Platform" nsiTemplate nsisFileProductFile fn
    ghcNsisFile %> \fn -> do
        need [ subInstNsiTemplate, commonNshFile ]
        expandAndCopy "GHC" subInstNsiTemplate ghcProductFile fn
    ghcDocNsisFile %> \fn -> do
        need [ subInstNsiTemplate, commonNshFile ]
        expandAndCopy "GHCDoc" subInstNsiTemplate ghcDocProductFile fn
    msysNsisFile %> \fn -> do
        need [ subInstNsiTemplate, commonNshFile ]
        expandAndCopy "MSys" subInstNsiTemplate msysProductFile fn
    extralibsNsisFile %> \fn -> do
        need [ subInstNsiTemplate, commonNshFile ]
        expandAndCopy "Extralibs" subInstNsiTemplate extralibsProductFile fn
    bootstrapNsisFile %> \fn -> do
        need [ bootstrapNsiTemplate, commonNshFile ]
        expandAndCopy "Bootstrap" bootstrapNsiTemplate osProduct fn

    -- Create the main doc/index.html from the template
    docIndexFile %> \_ -> do
        pCtx <- platformContext
        copyExpandedFile pCtx docIndexTmpl docIndexFile

    -- Build installer rules; makensis must be run in installerPartsDir
    ghcProductFile       %> \_ -> do
        need [ ghcNsisFile,  ghcNsisInstDat,  ghcNsisUninstDat ]
        command_ [Cwd installerPartsDir] "makensis" [ghcNsisFileName]
    ghcDocProductFile    %> \_ -> do
        need [ ghcDocNsisFile,  ghcDocNsisInstDat,  ghcDocNsisUninstDat ]
        command_ [Cwd installerPartsDir] "makensis" [ghcDocNsisFileName]
    msysProductFile      %> \_ -> do
        need [ msysNsisFile, msysNsisInstDat, msysNsisUninstDat ]
        command_ [Cwd installerPartsDir] "makensis" [msysNsisFileName]
    extralibsProductFile %> \_ -> do
        need [ extralibsNsisFile, extralibsNsisInstDat, extralibsNsisUninstDat ]
        command_ [Cwd installerPartsDir] "makensis" [extralibsNsisFileName]
    nsisFileProductFile %> \_ -> do
        need $ [dir ghcLocalDir, phonyTargetDir, vdir ghcVirtualTarget]

        copyWinTargetExtras

        -- Now, targetDir is actually ready to snapshot (we skipped doing
        -- this in osGhcTargetInstall).
        void $ getDirectoryFiles "" [targetDir ++ "//*"]

        need winNeeds

        -- Now, it is time to make sure there are no problems with the
        -- conf files copied to
        localCommand' [] "ghc-pkg"
            [ "recache"
            , "--package-db=" ++ winGhcTargetPackageDbDir ]
        localCommand' [] "ghc-pkg"
            [ "check"
            , "--package-db=" ++ winGhcTargetPackageDbDir ]

        -- in parallel, build the sub-installers
        need winSubProductFiles
        command_ [Cwd installerPartsDir] "makensis" [nsisFileName]

    osProduct            %> \_ -> do
        need [ nsisFileProductFile ]
        need [ bootstrapNsisFile ]
        -- Build main installer; makensis must be run in installerPartsDir
        command_ [Cwd installerPartsDir] "makensis" [bootstrapNsisFileName]

  where
    makeInstDat instFilter targDir dFile = do
        need [phonyTargetDir]
        dirs <- getDirsFiles instFilter targDir
        genData nsisInstDatTmpl dFile dirs
    makeUninstDat uninstFilter targDir uFile = do
        need [phonyTargetDir]
        dirs <- getDirsFiles uninstFilter targDir
        genData nsisUninstDatTmpl uFile dirs

    -- ghc bucket covers all directories and files not in msys, extralibs,
    -- or ghc's doc directory.
    --
    -- Since targetDir contains all the files in the entire release,
    -- we need to filter those paths out as they are covered by the
    -- msys, extralibs, ghcdoc .dat lists.  N.b.: Filtering by names could be
    -- fragile in light of any directory, package, structuring changes.
    -- Also note that even in for the "core" HP build, there are still
    -- a few things in extralibs (specifically alex and happy, as well as
    -- the cabal executable).
    ghcInstFilter = filterEmptyDirs . skipGHCDoc . skipExtralibs . skipMSys
    ghcUninstFilter =  sortByDirRev . skipGHCDoc . skipExtralibs . skipMSys

    -- For install, sort doesn't matter when parent vs child dir is created;
    -- For uninstall, deeper directories must be removed before their parent
    sortByDirRev = sortBy (flip (comparing fst))
        -- 'flip' comparing --> 'reverse'

    -- Filter directories with no files (but other directories are ok) for
    -- install but leave them for uninstall.
    filterEmptyDirs = filter (\(_,fs) -> not $ null fs)

    -- Note there filters any file with "doc" in the directory path, not just
    -- GHC doc, so this is nasty.  In this case, there is a "doc" within
    -- "extralibs" as well, but it is ok since we want no "extralibs" anyway.
    skipGHCDoc = filter (\(d, _) ->
        not $ any (equalFilePath "doc") (splitDirectories d))

    skipExtralibs = filter (\(d, _) ->
        not $ any (equalFilePath "extralibs") (splitDirectories d))

    skipMSys = filter (\(d, _) ->
        not $ any (equalFilePath "msys") (splitDirectories d))

    getDirsFiles f dr = liftIO $
        makeNativeRelPaths <$>
        f <$>
        getDirContentsR dr
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
    getDirContentSize f d = liftIO $
        (sum <$>) $
        join $ (mapM (\(_,fs) -> sum <$> mapM getFileSize fs)) <$>
        f <$>
        getDirContentsR d

    expandAndCopy nm templ pFile nFile = do
        stackFile <- askStackExe
        bc <- askBuildConfig
        pCtx <- platformContext
        mainSize <- getDirContentSize filterEmptyDirs winTargetDir
        let nsisCtx = expandNsisInfo bc ift
            ctx = nsisCtx `ctxAppend` pCtx
            stackSize = 48 * 1024 * 1024 -- not sure how to automatically determine
            ift = InfoForTemplate nm pFile nFile stackFile stackSize mainSize
        copyExpandedFile ctx templ nFile

-- | Expand the template, replacing following "keys" with values:
--   * key                 value
--   * osProductFileName   The final installer executable's filename
--   * productFile         The path (rooted at build) for this (sub-)installer
--   * productName         The string to use in UI for this (sub-)installer
--   * build64bit          Boolean: true if installer exe is 64-bit native
--   * is32or64            String: for UI to show "32" or "64"
--   * etc.
expandNsisInfo :: (Monad m) => BuildConfig -> InfoForTemplate ->
                  MuContext m
expandNsisInfo BuildConfig{..} InfoForTemplate{..} = mkStrContext ex
  where
    productFile = iftProdFile
        `relativeToDir` takeDirectory iftNSISFile
        -- NSIS tool needs to run from the installerPartsDir
    HpVersion{..} = bcHpVersion
    osProductFileName = winProductFileName bcIncludeExtra hpVersion bcArch

    ex "osProductFileName" = MuVariable osProductFileName
    ex "productFile" = MuVariable . toNative $ productFile
    ex "productName" = MuVariable iftProdName
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
    -- NSIS wants this in "kilobytes"
    inKB :: FileOffset -> Int
    inKB b = truncate $ (fromIntegral b) / (1024::Double)

copyWinTargetExtras :: Action ()
copyWinTargetExtras = do
    -- copy icons
    let mkIconsDir = makeDirectory $ winTargetDir </> "icons"
    copyFilesAction mkIconsDir winExtrasSrc winTargetDir winIconsFiles

    -- copy user's guide docs: ps, pdf, html, etc....
    makeDirectory winDocTargetDir

    ghcUgHtml <- askGhcUgHtml
    need [ghcUgHtml]
    command_ [Cwd winDocTargetDir]
        "tar" ["xf", ghcUgHtml `relativeToDir` winDocTargetDir]

    ghcLibsHtml <- askGhcLibs
    need [ghcLibsHtml]
    command_ [Cwd winDocTargetDir]
        "tar" ["xf", ghcLibsHtml `relativeToDir` winDocTargetDir]

    haddockHtml <- askHaddockHTML
    need [haddockHtml]
    command_ [Cwd winDocTargetDir]
        "tar" ["xf", haddockHtml `relativeToDir` winDocTargetDir]

    -- needContents winDocTargetDir -- needed here? is done by our caller, actually

    -- copy the PDF version of the GHC User's Guide
    -- (copyFilesAction does the 'need' on the PDF file)
    ghcUgPdf <- askGhcUgPDF
    need [ghcUgPdf]
    copyFileAction (return ()) (takeDirectory ghcUgPdf) winDocTargetDir
        (takeFileName ghcUgPdf)

    -- copy winghci pieces
    copyDirAction winExternalWinGhciDir winWinGhciTargetDir

    -- copy cabal executable
    cabalFile <- askCabalExe
    copyFileAction (return ()) (takeDirectory cabalFile) (winHpTargetDir </> "bin") (takeFileName cabalFile)
