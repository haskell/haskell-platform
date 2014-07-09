module GhcDist
    ( ghcDistRules
    )
  where

import Control.Applicative ((<$>))
import Data.List (intercalate, isInfixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Development.Shake
import Development.Shake.FilePath

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
