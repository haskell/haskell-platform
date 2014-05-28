module Utils where

import Data.Maybe (listToMaybe)
import Data.Version (Version, parseVersion)
import Development.Shake (Action, command_, liftIO, writeFileChanged)
import Development.Shake.FilePath
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)

version :: String -> Version
version s = case readMaybe (readP_to_S parseVersion) s of
    Just v -> v
    Nothing -> error $ "Error parsing '" ++ s ++ "' as a Version"


readMaybe :: ReadS a -> String -> Maybe a
readMaybe r = listToMaybe . map fst . filter (null . snd) . r


-- | Return a path, made useful relative to a directory.
-- This is used when the path will be used in a command, after changing the
-- working directory to the given dir.
--
-- Unlike `makeRelative` this correctly handles paths in any relation to each
-- other.
relativeToDir :: FilePath -> FilePath -> FilePath
fp `relativeToDir` dp = case (isRelative fp, isRelative dp) of
    (False, True) -> fp
    (True, False) -> pathError
    _ ->
        if fDrive == dDrive then relPath else error pathError
  where
    (fDrive, fPath) = splitDrive fp
    (dDrive, dPath) = splitDrive dp

    relPath = pathParts fPath `rel` pathParts dPath
    (f:fs) `rel` (d:ds) | f == d = fs `rel` ds
    [] `rel` [] = "."
    fs `rel` ds = joinPath $ (map (const "..") ds) ++ fs

    pathParts = map noTrailingSlash . filter (/= ".") . splitPath . normalise
    noTrailingSlash [] = []
    noTrailingSlash [c,'/'] = [c]
    noTrailingSlash (c:cs) = c : noTrailingSlash cs

    pathError = "relativeToDir: incompatible paths " ++ fp ++ " and " ++ dp

(®) :: FilePath -> FilePath -> FilePath
(®) = relativeToDir
infix 6 ®


-- | Place one path "under" another. If the second path is absolute, then it
-- is placed as if it were rooted in the first. If the second path is relative
-- then this is the same as (</>)
(</+>) :: FilePath -> FilePath -> FilePath
base </+> rel | isAbsolute rel = base ++ rel
              | otherwise      = base </> rel
infixr 5 </+>

-- | Return a path, made absoulte relative to the current directory.
-- If the path is already absolute, then it is returned directly.
absolutePath :: FilePath -> IO FilePath
absolutePath fp | isAbsolute fp = return fp
                | otherwise = (</> fp) `fmap` getCurrentDirectory


-- | Recursively remove a directory. Like shell command "rm -rf".
-- Unlike System.Directory.removeDirectoryRecursive, doesn't follow symlinks.
removeDirectoryRecursive :: FilePath -> Action ()
removeDirectoryRecursive fp = command_ [] "rm" [ "-Rf", "--", fp]

makeDirectory :: FilePath -> Action ()
makeDirectory fp = liftIO $ createDirectoryIfMissing True fp

writeFileLinesChanged :: FilePath -> [String] -> Action ()
writeFileLinesChanged fp = writeFileChanged fp . unlines

