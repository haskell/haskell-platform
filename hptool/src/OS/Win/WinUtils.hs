module OS.Win.WinUtils
    ( DirContents
    , getDirContents
    , getDirContentsR
    , parseConfFile
    , toCabalPrefix
    )
  where

import Control.Applicative ( (<$>), liftA )
import Control.Monad ( forM, unless )
import Data.Either ( partitionEithers )
import Development.Shake ( Action, putNormal )
import Development.Shake.FilePath ( toNative, (</>) )
import qualified Distribution.InstalledPackageInfo as C
import qualified System.Directory ( doesDirectoryExist, getDirectoryContents )


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
