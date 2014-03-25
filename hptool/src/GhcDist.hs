module GhcDist
    ( localCommand, localCommand'
    , ghcDistRules
    )
  where

import Control.Applicative ((<$>))
import Data.List (intercalate)
import Development.Shake
import Development.Shake.FilePath
import System.Environment (getEnvironment)

import Config
import Dirs
import OS
import Paths
import Types
import Utils

ghcInstall :: FilePath -> Maybe (BuildConfig -> FilePath) -> Action FilePath
ghcInstall base mfPrefix = do
    tarFile <- askGhcBinDistTarFile
    conf <- askBuildConfig
    let distDir = ghcBinDistDir $ bcGhcVersion conf
        untarDir = takeDirectory distDir
        (prefix, destDir) = layout (($ conf) <$> mfPrefix)
        destArg = maybe [] (\_ -> ["DESTDIR=" ++ base ® distDir]) mfPrefix

    makeDirectory untarDir

    command_ [Cwd untarDir]
        "tar" ["-jxf", tarFile ® untarDir]

    configCmd <- liftIO $ absolutePath $ distDir </> "configure"
    absPrefix <- liftIO $ absolutePath $ prefix

    command_ [Cwd distDir]
        configCmd ["--prefix=" ++ absPrefix]
    command_ [Cwd distDir]
        "make" (["install"] ++ destArg)

    return destDir
  where
    layout Nothing = (base, base)
    layout (Just p) = (p, base </+> p)

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
                        else doesFileExist localCmd
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
