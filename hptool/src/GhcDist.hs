module GhcDist
    ( localCommand, localCommand'
    , ghcDistRules
    )
  where

import Control.Applicative ((<$>))
import Data.List (intercalate, isInfixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Development.Shake
import Development.Shake.FilePath
import System.Environment (getEnvironment)

import Config
import Dirs
import OS
import Paths
import Types
import Utils

-- The ordering of these constructors is significant, it is used by
-- getCppCommand's heuristics to figure out the cpp by --version output
data CPPCommand = CPP_clang
                | CPP_gcc
                | CPP_cpphs
                deriving (Show, Eq, Ord, Enum, Bounded)

type GHCSettings = [(String, String)]

cppCommandName :: CPPCommand -> String
cppCommandName cpp = case cpp of
    CPP_clang -> "clang"
    CPP_gcc   -> "gcc"
    CPP_cpphs -> "cpphs"

cppCommandFlags :: CPPCommand -> String
cppCommandFlags cpp = case cpp of
    CPP_clang -> "-E -P -undef -traditional -Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs"
    CPP_gcc   -> "-E -undef -traditional"
    CPP_cpphs -> "--cpp -traditional"

ghcInstall :: FilePath -> Maybe (BuildConfig -> FilePath) -> Action FilePath
ghcInstall base mfPrefix = do
    tarFile <- askGhcBinDistTarFile
    conf <- askBuildConfig
    let distDir = ghcBinDistDir $ bcGhcVersion conf
        untarDir = takeDirectory distDir
        (prefix, destDir) = layout (($ conf) <$> mfPrefix)
        destArg = maybe [] (\_ -> ["DESTDIR=" ++ base ® distDir]) mfPrefix
        settingsFile = destDir </> "lib" </> show (bcGhcVersion conf) </> "settings"

    makeDirectory untarDir

    command_ [Cwd untarDir]
        "tar" ["xf", tarFile ® untarDir]

    configCmd <- liftIO $ absolutePath $ distDir </> "configure"
    absPrefix <- liftIO $ absolutePath $ prefix

    command_ [Cwd distDir]
        configCmd ["--prefix=" ++ absPrefix]
    command_ [Cwd distDir]
        "make" (["install"] ++ destArg)

    settings <- readSettings settingsFile
    Just cppCommand <- getCppCommand settings settingsFile
    writeSettings settingsFile (updateCppFlags cppCommand settings)

    return destDir
  where
    layout Nothing = (base, base)
    layout (Just p) = (p, base </+> p)

readSettings :: FilePath -> Action GHCSettings
readSettings settingsFile = read <$> liftIO (readFile settingsFile)

updateCppFlags :: CPPCommand -> GHCSettings -> GHCSettings
updateCppFlags cppCommand = map replaceFlags
    where
        replaceFlags (k, v) = case k of
            "Haskell CPP flags"  -> (k, cppCommandFlags cppCommand)
            _                    -> (k, v)

writeSettings :: FilePath -> GHCSettings -> Action ()
writeSettings settingsFile settings = writeFile' settingsFile $
    "[ " ++ intercalate "\n, " (map show settings) ++ "\n]"

getCppCommand :: GHCSettings -> FilePath -> Action (Maybe CPPCommand)
getCppCommand settings settingsFile = do
    let cppCommand = fromMaybe
            (error $ "Haskell CPP command not found in " ++ settingsFile)
            (lookup "Haskell CPP command" settings)
    -- It's possible we could use a better heuristic here for cpphs, if that is
    -- ever used in the future we may be able to simply look at the basename
    -- of cppCommand.
    (Stdout cppVersion, Stderr _) <- command [] cppCommand ["--version"]
    return . listToMaybe $
        filter ((`isInfixOf` cppVersion) . cppCommandName) [minBound .. maxBound]

ghcDistRules :: Rules ()
ghcDistRules = do
    ghcLocalDir */> \_ -> do
        ghcInstall ghcLocalDir Nothing >> return ()
    ghcVirtualTarget ~/> do
        ghcInstall targetDir (Just targetPrefix) >>= return . Just
  where
    targetPrefix = osGhcPrefix . osFromConfig

-- TODO(mzero): need a way to ensure that multiple uses of bindist don't
--              happen in parallel


-- | Run a command, but favor the local GHC instalation.
-- Note that 'command' ends up calling 'createProcess', which in turn in this
-- case call 'execvpe()' in C. That call isn't part of Posix, it is supplied
-- by the unix library from GHC. That code chooses to use the PATH in the
-- calling ENV, not the supplied environment for the new process.
-- Hence, this code here has to look up to see if the command should be in the
-- bin dir. It also needs to modify the path so that all commands called from
-- this command will see the bin dir as well.
localCommand :: CmdResult r => [CmdOption] -> String -> [String] -> Action r
localCommand opts cmdName args = do
    need [ dir ghcLocalDir ]
    let localBin = ghcLocalDir </> "bin"
    absLocalBin <- liftIO $ absolutePath localBin
    localPath <- addPath' [absLocalBin] []
    let localCmd = absLocalBin </> cmdName
    useLocalCmd <- if '/' `elem` cmdName
                        then return False
                        else doesFileExist $ localCmd <.> exe
    command (localPath : opts) (if useLocalCmd then localCmd else cmdName) args

localCommand' :: [CmdOption] -> String -> [String] -> Action ()
localCommand' = localCommand

-- | Fixed addPath
addPath' :: [String] -> [String] -> Action CmdOption
addPath' pre post = do
    args <- liftIO getEnvironment
    return $ Env $ map (onPath updatePath) $ ensurePath args
  where
    onPath f e@(a, b) | a == "PATH" = (a, f b)
                      | otherwise = e
    updatePath p = intercalate [searchPathSeparator] $ pre ++ [p | p /= ""] ++ post
    ensurePath e = maybe (("PATH", ""):e) (const e) $ lookup "PATH" e
