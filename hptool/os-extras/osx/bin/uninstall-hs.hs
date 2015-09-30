#!/usr/local/bin/env runghc

module Main where

{-
    Uninstall.hs - a Haskell uninstaller for Mac OS X

    This program is really far too big to be in a single file. However, I
    wanted it to be easily distributable and runnable, and so have kept it all
    together.

    - Mark Lentczner
-}

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Exception (catch, IOException)
import Control.Monad ((>=>), msum, when)
import Data.Char (isDigit)
import Data.List (foldl', intercalate, isInfixOf, isPrefixOf, nub, sort)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, doesFileExist,
    getDirectoryContents)
import System.Environment (getArgs, getEnvironment, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), joinPath, splitDirectories, takeDirectory,
    takeFileName)
import System.IO (hPutStrLn, stderr)
import System.Posix.Directory (removeDirectory)
import System.Posix.Files (createSymbolicLink, getSymbolicLinkStatus,
    isSymbolicLink, isDirectory, readSymbolicLink, removeLink, rename)
import System.Process (readProcess)


--
-- Utilities
--

-- | Break a list apart into sections separated by a delimiter element
parts :: Eq a => a -> [a] -> [[a]]
parts d s = case break (== d) s of
    ([], []) -> []
    (a, []) -> [a]
    (a, (_:b)) -> a : parts d b

-- | Contents of a directory. Like getDirectoryContents, only a) safe, returning
-- [] if there is a problem, and b) excludes "." and ".."
contents :: FilePath -> IO [FilePath]
contents fp =
    filter notSpecial `fmap` (getDirectoryContents fp `catchIO` (\_ -> return []))
  where
    notSpecial :: String -> Bool
    notSpecial n = not $ n `elem` [".", ".."]

-- | Entries under a directory. Like contents, but with the dir path prepended.
entries :: FilePath -> IO [FilePath]
entries fp = map (fp </>) `fmap` contents fp

-- | FilePath doesn't start with a dot
notDot :: FilePath -> Bool
notDot = not . ("." `isPrefixOf`) . takeFileName

-- | simplifyPath path, elminiating . and .. components (if possible)
simplifyPath :: FilePath -> FilePath
simplifyPath = joinPath . simp [] . splitDirectories
  where
    simp    ys        []  = reverse ys
    simp    ys  ( ".":xs) = simp    ys  xs
    simp (y:ys) ("..":xs)
          | y /= ".."     = simp    ys  xs
    simp    ys  (   x:xs) = simp (x:ys) xs

-- | A version of `catch` that catches `IOException`, and hence any exception
-- at all.
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

--
-- Version Numbers
--

data Version = Version [Integer] String
  deriving (Eq, Ord)

instance Show Version where
  show (Version ns x) = intercalate "." $
                            map show ns ++ if null x then [] else [x]

version :: String -> Maybe Version
version s = case vparts s of
    ([], _)                         -> Nothing
    ([m], x) | m >= 600 && m < 800  -> Just $ let (a, b) = m `divMod` 100 in
                                                    Version [a, b] x
             | otherwise            -> Nothing
        -- some old versions were installed in directories named "610" and "612"
    (ns, x)                         -> Just $ Version ns x
  where
    vparts s' = case span isDigit s' of
        ("", x) -> ([], x)
        (n, ('.':r)) -> let (m, x) = vparts r in (read n:m, x)
        (n, x) -> ([read n], x)

ghcVersion :: String -> Maybe Version
ghcVersion s = case parts '-' s of
    ("ghc":v:_) -> version v
    _ -> Nothing

partVersion :: String -> Maybe Version
partVersion = msum . map version . parts '-'


data VersionTest = VersionAll | VersionOnly Version
                 | VersionUpto Version | VersionThru Version
    deriving (Eq)

versionTest :: VersionTest -> Version -> Bool
versionTest rt = case rt of
    VersionAll -> const True
    (VersionOnly v) -> (v ==)
    (VersionUpto v) -> (v >)
    (VersionThru v) -> (v >=)



--
-- Find Arrow: Finding things in the file system
--

-- | A Find takes an annotated FilePath to a list of annotated FilePaths
-- The annotations in and out can differ.
data Find a b = Find { unFind :: (a, FilePath) -> IO [(b, FilePath)] }

instance Category Find where
    id = Find $ return . return
    fbc . fab = Find $ unFind fab >=> fmap concat . mapM (unFind fbc)

instance Arrow Find where
    arr f = Find $ \(a, fp) -> return [(f a, fp)]
    first fab = Find $ \((a, x), fp) ->
        unFind fab (a, fp) >>= return . map (\(b, fp') -> ((b, x), fp'))

runFind :: Find () a -> IO [(a, FilePath)]
runFind fua = unFind fua ((), "/")

runFinds :: [Find () a] -> IO [(a, FilePath)]
runFinds = fmap concat . mapM runFind

path :: FilePath -> Find a a
path p = Find $ \(a, f) -> return [(a, f </> p)]

star :: Find a a
star = Find $ \(a, fp) -> entries fp >>= return . map (\gp -> (a, gp))


fileTest :: (FilePath -> IO Bool) -> Find a a
fileTest p =
    Find $ \(a, fp) -> p fp >>= return . (\b -> if b then [(a, fp)] else [])

fileExtract :: (a -> FilePath -> IO (Maybe b)) -> Find a b
fileExtract p =
    Find $ \(a, fp) -> p a fp >>= return . maybe [] (\b -> [(b, fp)])

exists :: Find a a
exists = fileTest $ \fp -> do
    dde <- doesDirectoryExist fp
    dfe <- doesFileExist fp
    return $ dde || dfe

fileExists :: Find a a
fileExists = fileTest doesFileExist

dirExists :: Find a a
dirExists = fileTest doesDirectoryExist


findFilter :: (a -> FilePath -> Maybe b) -> Find a b
findFilter p = Find $ \(a, fp) -> return $ maybe [] (\b -> [(b, fp)]) $ p a fp

test :: (a -> Bool) -> Find a a
test p = findFilter $ \a _fp -> if p a then Just a else Nothing

match :: (FilePath -> Bool) -> Find a a
match p = findFilter $ \a fp -> if p fp then Just a else Nothing

extract :: (FilePath -> Maybe b) -> Find a b
extract p = findFilter $ const p

matches :: (FilePath -> Bool) -> Find a a
matches p = star >>> match (p . takeFileName)

extracts :: (FilePath -> Maybe b) -> Find a b
extracts p = star >>> extract (p . takeFileName)


--
-- Finds for various places where Haskell bits are stored
--

ghcName :: FilePath -> Bool
ghcName = isJust . ghcVersion

-- | Find all the per-version installation directories.
findVersions :: IO (Map.Map Version [FilePath])
findVersions = makeMap `fmap` runFinds
    [ path "/Library/Frameworks/GHC.framework/Versions"                 >>> extracts partVersion
    , path "/Library/Frameworks/HaskellPlatform.framework/lib" >>> star >>> extracts ghcVersion
    , path "/Library/Haskell"                                           >>> extracts ghcVersion
    , path "/Users" >>> star >>> path ".cabal/lib" >>> star             >>> extracts ghcVersion
    , path "/Users" >>> star >>> path ".ghc"                            >>> extracts partVersion
    , path "/Users" >>> star >>> path "Library/Haskell"                 >>> extracts ghcVersion
    , path "/usr/local/lib"                                             >>> extracts ghcVersion
    , path "/usr/local/lib" >>> matches (not . ghcName)                 >>> extracts ghcVersion
    ]
  where
    makeMap :: Ord a => [(a, b)] -> Map.Map a [b]
    makeMap = Map.fromListWith (++) . map (\(a, b) -> (a, [b]))

-- | Find all the top level installation directories. Includes some per-version
-- directories where things were stored in common system lib directories.
findAll :: IO [FilePath]
findAll = map snd `fmap` runFinds
    [ path "/Library/Frameworks/GHC.framework"                          >>> exists
    , path "/Library/Frameworks/HaskellPlatform.framework"              >>> exists
    , path "/Library/Haskell"                                           >>> exists
    , path "/Users" >>> star >>> path ".cabal" >>> matches (excludePrefix "config")
    , path "/Users" >>> star >>> path ".ghc"   >>> matches (excludePrefix "ghci")
    , path "/Users" >>> star >>> path "Library/Haskell"                 >>> exists
    , path "/usr/local/lib" >>> matches ghcName
    , path "/usr/local/lib" >>> matches (not . ghcName) >>> matches ghcName
    ]
  where
    excludePrefix :: String -> FilePath -> Bool
    excludePrefix p fp = not $ p `isPrefixOf` fp

-- | Find symlinks on the PATH that point into directories that are going to be
-- removed.
findOrphanSymlinks :: [FilePath] -> IO [FilePath]
findOrphanSymlinks removed = do
    pathDirs <- (maybe [] (parts ':') . lookup "PATH") `fmap` getEnvironment
    let placesToLook =
            map path (pathDirs ++ [ "/usr/bin", "/usr/local/bin" ])
            ++ [ path "/Users" >>> star >>> path "Library/Haskell/bin" ]
    (nub . map snd) `fmap` runFinds
        (map (\p -> p >>> star >>> sym >>> test orphan) placesToLook)
  where
    sym :: Find a FilePath
    sym = fileExtract $ const $ \fp -> do
        st <- getSymbolicLinkStatus fp
        if isSymbolicLink st
            then (Just . simplifyPath . (takeDirectory fp </>))
                    `fmap` readSymbolicLink fp
            else return Nothing
    orphan fp = any (`isPrefixOf` fp) removed

-- | Find all package directories where removing the per-version directory
-- might indicate that the whole package can be removed.
findEmptyPackages :: VersionTest -> IO [(Bool, FilePath)]
findEmptyPackages rt = libVersions >>= fmap catMaybes . mapM willEmpty
  where
    libVersions = map snd `fmap` runFinds packageFind

    packageFind = case rt of
        VersionAll -> packagesToAlwaysCheck
        _          -> packagesToAlwaysCheck ++ packagesCoveredByAll
    packagesToAlwaysCheck =
        [ path "/usr/local/lib" >>> matches (not . ghcName) ]
    packagesCoveredByAll =
        [ path "/Library/Frameworks/HaskellPlatform.framework/lib" >>> star
        , path "/Users" >>> star >>> path ".cabal/lib" >>> star
        ]

    willEmpty :: FilePath -> IO (Maybe (Bool, FilePath))
    willEmpty fp = do
        names <- filter notDot `fmap` contents fp
        let ghcVersions = catMaybes $ map ghcVersion names
        let removingAll = all (versionTest rt) ghcVersions
        let namesLeft = filter (not . willRemove) names
        return $ if not (null ghcVersions) && removingAll
            then Just (null namesLeft, fp)
            else Nothing

    willRemove = maybe False (versionTest rt) . ghcVersion

--
-- Program Options
--

data OptRemove = OptDryRun | OptScript | OptRemove
    deriving (Eq, Ord)

data Options = Options { optVerbose, optHelp :: Bool,
                         optRemove :: OptRemove }
optReportRemove :: Options -> Bool
optReportRemove opts = case optRemove opts of
    OptDryRun -> True
    OptScript -> False
    OptRemove -> optVerbose opts

optionsDescr :: [OptDescr (Options -> Options)]
optionsDescr =
    [ Option ['v'] ["verbose"]      (NoArg setVerbose)  "report each path"
    , Option ['n'] ["dry-run"]      (NoArg setDryRun)   "only report what would be removed"
    , Option ['s'] ["sh", "script"] (NoArg setScript)   "generate a shell script to remove files"
    , Option ['r'] ["rm", "remove"] (NoArg setRemove)   "actually remove files"
    , Option ['?'] ["help"]         (NoArg setHelp)     "help (this message)"
    ]
  where
    setVerbose opts = opts { optVerbose = True }
    setDryRun opts = opts { optRemove = OptDryRun }
    setScript opts = opts { optRemove = OptScript }
    setRemove opts = opts { optRemove = OptRemove }
    setHelp opts = opts { optHelp = True }

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv =
   case getOpt Permute optionsDescr argv of
      (o,n,[]  ) -> return (foldl' (flip ($)) defaultOpts o,n)
      (_,_,errs) -> usageFailure (concat errs)
  where
    defaultOpts =
        Options { optVerbose = False, optHelp = False,
                  optRemove = OptDryRun }

progMessage :: String -> IO ()
progMessage msg = do
    prog <- getProgName
    putStr $ intercalate prog $ parts '$' msg

usage :: IO ()
usage = do
    progMessage header
    putStr $ usageInfo "Options (can appear anywhere):" optionsDescr
  where
    header =
        "Usage: $              -- find versions on system\n\
        \       $ thru VERSION -- remove VERSION and earlier\n\
        \       $ only VERSION -- remove only VERSION\n\
        \       $ all          -- remove all\n\
        \NOTE: Commands are 'dry run' by default and don't actually delete.\n"

usageFailure :: String -> IO a
usageFailure msg = do
    mapM_ (putStrLn . ("*** " ++)) $ lines msg
    usage
    exitFailure

message :: Options -> String -> IO ()
message opts str = putStrLn $ messagePrefix ++ str
  where
    messagePrefix = if (optRemove opts == OptScript) then "echo " else ""


--
-- Primitive File Operations
--

safely :: FilePath -> IO () -> IO ()
safely fp = (`catchIO` (hPutStrLn stderr . fmt . show))
  where
    fmt msg = "** ERROR "
        ++ (if fp `isInfixOf` msg then "" else fp ++ ": ") ++ msg

-- | Recursively remove a directory. Like shell command "rm -rf".
-- Unlike System.Directory.removeDirectoryRecursive, doesn't follow symlinks.
removeDirectoryRecursive :: Options -> FilePath -> IO ()
removeDirectoryRecursive opts fp = do
    when (optReportRemove opts) $ putStrLn fp
    case (optRemove opts) of
        OptDryRun -> return ()
        OptScript -> putStrLn ("rm -rf " ++ fp)
        OptRemove -> rmrf fp
  where
    rmrf f = do
        st <- getSymbolicLinkStatus f
        if isDirectory st
            then do
                entries f >>= mapM_ rmrf
                safely f $ removeDirectory f
            else
                safely f $ removeLink f

-- | Remove a file. Like shell command "rm -f".
-- If file is a symlinks, removes the symlink, not what it points to.
removeFile :: Options -> FilePath -> IO ()
removeFile opts fp = do
    when (optReportRemove opts) $ do
        st <- getSymbolicLinkStatus fp
        if isSymbolicLink st
            then readSymbolicLink fp >>= putStrLn . ((fp ++ "@ -> ") ++)
            else putStrLn fp
    case (optRemove opts) of
        OptDryRun -> return ()
        OptScript -> putStrLn ("rm -f " ++ fp)
        OptRemove -> safely fp $ removeLink fp

-- | Symlink a file. Like shell command "ln -sf".
-- If file is a symlinks, removes the symlink, not what it points to.
symlinkFile :: Options -> FilePath -> FilePath -> IO ()
symlinkFile opts dest fp = do
    when (optReportRemove opts) $
        putStrLn (fp ++ "@ update to -> " ++ dest)
    case (optRemove opts) of
        OptDryRun -> return ()
        OptScript -> putStrLn ("ln -sf " ++ dest ++ " " ++ fp)
        OptRemove -> safely fp $ removeLink fp >> createSymbolicLink dest fp

-- | Archive a file, by giving it a suffix with a unique integer attached
archiveFile :: Options -> String -> FilePath -> IO ()
archiveFile opts suffix fp = do
    dest <- findFreeArchive 0
    when (optReportRemove opts) $
        putStrLn (fp ++ " rename to -> " ++ dest)
    case (optRemove opts) of
        OptDryRun -> return ()
        OptScript -> putStrLn ("mv " ++ fp ++ " " ++ dest)
        OptRemove -> safely fp $ rename fp dest
  where
    findFreeArchive :: Int -> IO FilePath
    findFreeArchive n = do
        let dest = fp ++ suffix ++ "." ++ show n
        dfe <- doesFileExist dest
        if dfe
            then findFreeArchive (n + 1)
            else return dest

-- | For each framework, update the Current symlink if the version it points
-- to will be removed, or remove the whole framework if nothing will be left.
updateFrameworks :: Options -> VersionTest -> IO ()
updateFrameworks opts rt = when (rt /= VersionAll) $
    mapM_ updateFramework frameworks
  where
    frameworks =
        [ ("/Library/Frameworks/GHC.framework", "Versions", "Current")
        , ("/Library/Haskell", "", "current")
        ]
    updateFramework (fp, vp, cp) = do
        items <- contents $ fp </> vp
        let remain = filter (willKeep cp) items
        let remainVers = reverse . sort . mapMaybe andVersion $ remain

        let curr = fp </> vp </> cp
        currDest <- readSymbolicLink curr `catchIO` (\_ -> return "")
        when (willRemove currDest) $ case (remain, remainVers) of
            ([], _) ->     -- nothing will remain, remove whole framework
                removeDirectoryRecursive opts fp
            (_, []) -> do  -- no versions will remain, but something will
                removeFile opts curr
                message opts $ "** " ++ fp ++
                    " is not empty, but has no more versions. Consider removing."
            (_, ((_,newDest):_)) ->  -- update to maximal remaining version
                symlinkFile opts newDest curr

    willRemove = maybe False (versionTest rt) . partVersion
    willKeep cp fp = notDot fp && (fp /= cp) && (not $ willRemove fp)
    andVersion fp = (\v -> (v, fp)) `fmap` partVersion fp


--
-- Main Operations
--

-- | Display versions found
showVersions :: Options -> Map.Map Version [FilePath] -> IO ()
showVersions opts m = do
    whenVer blank
    mapM_ disp (Map.toAscList m)
    progMessage hints
  where
    whenVer = when (optVerbose opts)
    blank = putStrLn ""
    disp (v, fp) = do
        putStrLn $ show v
        whenVer $ do
            mapM_ (putStrLn . ("    " ++)) $ sort fp
            blank
    hints =
        "-- To remove a version and all earlier: $ thru VERSION\n\
        \-- To remove only a single version:     $ only VERSION\n\n"

alertOlderVersions :: String -> Map.Map Version [FilePath] -> IO ()
alertOlderVersions appl m = when (not $ Map.null m) $ do
    _ <- readProcess "osascript" [] alert
    return ()
  where
    alert = "tell application \"" ++ appl ++ "\"\n\
            \\tactivate\n\
            \\tdisplay alert \"Older Versions\" message \"" ++ msg ++ "\"\n\
            \end tell\n"
    msg = "There are older versions of GHC and/or \
          \Haskell Platform on this system.\r\
          \\r\
          \Run the command line tool uninstall-hs to \
          \find out which and how to remove them."

-- | Remove file paths and associated other files.
-- Must be supplied the predicate used to select versions to remove so that the
-- associated files can be correctly identified.
remove :: Options -> VersionTest -> [FilePath] -> IO ()
remove opts rt fps = do
    case sort fps of
        [] -> message opts "** Nothing to remove"
        sfps -> do
            mapM_ (removeDirectoryRecursive opts) sfps
            findOrphanSymlinks fps >>= mapM_ (removeFile opts)
            findEmptyPackages rt >>= mapM_ removePackage
            updateFrameworks opts rt
            removeHints
  where
    removePackage (empty, fp) = do
        if empty
            then removeDirectoryRecursive opts fp
            else message opts
                ("** " ++ fp ++
                 " is not empty, but has no more GHC libs. Consider removing.")

    removeHints = when (optRemove opts == OptDryRun) $
        putStrLn
            "-- To actually remove these files, \
                \sudo run the command again with --remove\n\
            \-- To generate a script to remove these files, \
                \run the command again with --script\n"

-- | Remove all Haskell versions, and the top level directories.
removeAll :: Options -> IO ()
removeAll opts = do
    runFind cabalConfigs >>= mapM_ (archiveFile opts ".orig" . snd)
    findAll >>= remove opts VersionAll
  where
    cabalConfigs = path "/Users" >>> star >>> path ".cabal/config" >>> exists



main :: IO ()
main = getArgs >>= parseOptions >>= uncurry main'

main' :: Options -> [String] -> IO ()
main' opts args = do
    when (optHelp opts) $ usage >> exitSuccess

    case args of
      [] -> do
        putStrLn "-- Versions found on this system"
        findVersionsThat VersionAll >>= showVersions opts

      ["all"] -> do
        removePlan "all Haskell directories"
        removeAll opts

      ["test"] -> do
        main' testOpts []
        vers <- Map.keys `fmap` findVersions
        mapM_ (\v -> main' testOpts ["only", show v]) vers
        mapM_ (\v -> main' testOpts ["thru", show v]) vers
        main' testOpts ["all"]

      ["thru", v] -> withVersion v $ \ver -> do
        removePlan $ "version " ++ show ver ++ " and earlier"
        removeVersionsThat (VersionThru ver)

      ["only", v] -> withVersion v $ \ver -> do
        removePlan $ "just version " ++ show ver
        removeVersionsThat (VersionOnly ver)

      ["install-check", v, a] -> withVersion v $ \ver -> do
        findVersionsThat (VersionUpto ver) >>= alertOlderVersions a

      _ -> usageFailure "unregcognized args"

  where
    removePlan s = message opts $ removePrefix ++ s
    removePrefix = case optRemove opts of
        OptDryRun -> "-- Would remove "
        _         -> "-- Removing "

    withVersion v a =
        maybe (usageFailure "couldn't parse version") a $ version v

    findVersionsThat rt =
        Map.filterWithKey (const . versionTest rt) `fmap` findVersions

    removeVersionsThat rt =
        findVersionsThat rt >>= remove opts rt . concat . Map.elems

    testOpts = opts { optVerbose = True, optRemove = OptDryRun }
