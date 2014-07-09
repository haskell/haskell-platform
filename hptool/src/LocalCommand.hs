module LocalCommand
    ( localCommand, localCommand'
    )
  where

import Data.List (intercalate)
import Development.Shake (Action, CmdOption(..), CmdResult, command,
                          doesFileExist, liftIO, need)
import Development.Shake.FilePath ( (</>), (<.>), exe, searchPathSeparator )
import System.Environment (getEnvironment)

import Dirs
import Paths
import Utils


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

