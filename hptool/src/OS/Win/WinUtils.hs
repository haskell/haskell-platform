module OS.Win.WinUtils
    ( DirContents
    , copyDirAction
    , copyFileAction
    , copyFilesAction
    , getDirContents
    , getDirContentsR
    , parseConfFile
    , toCabalPrefix
    )
  where

import Control.Applicative ( (<$>), liftA )
import Control.Monad ( forM, unless )
import Data.Either ( partitionEithers )
import Development.Shake ( Action, need, putNormal, command_ )
import Development.Shake.FilePath ( toNative, (</>) )
import qualified Distribution.InstalledPackageInfo as C
import qualified System.Directory ( doesDirectoryExist, getDirectoryContents )

import Dirs ( needContents )
import Utils ( makeDirectory )

type DirContents = [(FilePath, [FilePath])]

getDirContents :: FilePath -> IO ([FilePath], [FilePath])
getDirContents topPath = do
  -- Avoid creating Shake dependencies by using System.Directory
  names <- System.Directory.getDirectoryContents topPath
  let properNames = filter (`notElem` [".", ".."]) names
  liftA partitionEithers <$> forM properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- System.Directory.doesDirectoryExist path
    return $ if isDirectory
             then Left path
             else Right path

-- | From the path of a directory, recursively enumerate its contents,
--   eliding "." and ".." along the way, returning a list of pairs, one
--   pair for each directory encountered from the input; each pair is
--   comprised of the path to a particular directory, along with a list of
--   the names of that directory's contents (file- and directory-names).
getDirContentsR :: FilePath -> IO DirContents
getDirContentsR topPath = do
    (dirs,files) <- getDirContents topPath
    ((topPath,files) :) <$> concat <$> (mapM getDirContentsR dirs)

parseConfFile :: FilePath -> String -> Action C.InstalledPackageInfo
parseConfFile confFile conf =
    case C.parseInstalledPackageInfo conf of
        C.ParseFailed e ->
            error $ "Error while parsing " ++ confFile ++ ":\n" ++ show e
        C.ParseOk ws a -> do
            unless (null ws) $
                putNormal $ "Parsing " ++ confFile ++ " caused warnings:\n" ++
                    (unlines . map show $ ws)
            return a

-- Cabal on Windows requires an absolute, native-format prefix.
toCabalPrefix :: FilePath -> FilePath
toCabalPrefix = toNative . ("C:/" ++)

copyFileAction :: Action () -> FilePath -> FilePath -> FilePath -> Action ()
copyFileAction setup srcDir dstDir file = do
    need [srcDir </> file]
    setup
    command_ [] "cp" ["-p", srcDir </> file, dstDir </> file]

copyFilesAction :: Action () -> FilePath -> FilePath -> [FilePath] -> Action ()
copyFilesAction setup srcDir dstDir files = do
    setup
    mapM_ (copyFileAction (return ()) srcDir dstDir) files

copyDirAction :: FilePath -> FilePath -> Action ()
copyDirAction srcDir dstDir = do
    needContents srcDir
    makeDirectory dstDir
    -- Two problems: seems that (</>) strips the "." out, so use (++);
    -- second problem is that using an "*" in the path results in an error,
    -- so "/." works better.
    command_ [] "cp" ["-pR", srcDir ++ "/.", dstDir]
    needContents dstDir -- is this 'need' here correct? we do this in Win.hs
