module Utils where

import Control.Monad (when)
import Data.Maybe (listToMaybe)
import Data.Version (Version, parseVersion)
import Development.Shake (Action, command_, getVerbosity, liftIO, putNormal,
                          Verbosity(..), writeFileChanged)
import Development.Shake.FilePath
import qualified Distribution.Verbosity as Cabal
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import qualified System.Directory (doesDirectoryExist)


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

    pathParts = map noTrailingSlash . filter (/= ".") . splitPath . toStandard . normalise
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
-- If this already exists, it could indicate some build dependeny problems
-- so let's note those for tracking down.
removeDirectoryRecursive :: FilePath -> Action ()
removeDirectoryRecursive fp = do
    -- Use System.Directory to avoid creating a Shake dependency
    exists <- liftIO $ System.Directory.doesDirectoryExist fp
    when exists
        (putNormal $ "WARNING: Removing non-empty directory \""++fp++"\"")
    command_ [] "rm" [ "-Rf", "--", fp]

makeDirectory :: FilePath -> Action ()
makeDirectory fp = liftIO $ createDirectoryIfMissing True fp

writeFileLinesChanged :: FilePath -> [String] -> Action ()
writeFileLinesChanged fp = writeFileChanged fp . unlines

-- ghc-pkg command line verbosity ranges from 0..2, with 1 the default
shakeToGhcPkgVerbosity :: Action Int
shakeToGhcPkgVerbosity = do
    sV <- getVerbosity
    return $ case sV of
                 Silent     -> 0
                 Quiet      -> 0
                 Normal     -> 1
                 Loud       -> 1
                 Chatty     -> 2
                 Diagnostic -> 2

-- The cabal command line verbosity ranges from 0..3, with 1 the default.
-- The Cabal API mirrors the cabal command line settings.  Use fromEnum
-- to convert Cabal.Verbosity to an integer for use with the cabal tool.
shakeToCabalVerbosity :: Action Cabal.Verbosity
shakeToCabalVerbosity = do
    sV <- getVerbosity
    return $ case sV of
                 Silent     -> Cabal.silent
                 Quiet      -> Cabal.silent
                 Normal     -> Cabal.normal
                 Loud       -> Cabal.verbose
                 Chatty     -> Cabal.verbose
                 Diagnostic -> Cabal.deafening
