{-# LANGUAGE RecordWildCards #-}

module Templates
    ( ContextMaker, Expander
    , releaseContext, buildConfigContext, platformContext
    , platformExpander
    , copyExpandedFile, copyExpandedDir
    )
  where

import Control.Applicative ( (<$>) )
import Control.Monad (forM_, unless)
import qualified Data.Text.Lazy.IO as TL
import Data.Version (showVersion)
import Development.Shake
import Development.Shake.FilePath
import System.Directory (copyPermissions)
import Text.Hastache
import Text.Hastache.Context

import Config
import PlatformDB
import Types
import Utils

type Expander m = String -> MuType m

expandError :: Expander m
expandError t = error $ "expandError: unexpected template tag " ++ t


expandRelease :: (Monad m) => Release -> Expander m -> Expander m
expandRelease rel defEx = ex
  where
    ex "hpVersion" = MuVariable . showVersion . hpVersion . relVersion $ rel
    ex "ghcVersion" = case pkgsThat [isGhc, not . isLib] of
        [] -> error "No ghc version spec'd in release."
        [ghcPkg] -> MuVariable . showVersion . pkgVersion $ ghcPkg
        _ -> error "More than one ghc version spec'd in release."

    ex "ghcLibs"           = exPkgs $ pkgsThat [isGhc, isLib]
    ex "nonWindowsLibs"    = exPkgs $ pkgsThat [isNotWindows]
    ex "onlyWindowsLibs"   = exPkgs $ pkgsThat [isWindows]
    ex "platformLibs"      = exPkgs $ pkgsThat [not . isGhc, isLib]
    ex "tools"             = exPkgs $ pkgsThat [isTool]

    ex t = defEx t

    pkgsThat tests = packagesByIncludeFilter (\i -> all ($i) tests) rel

    exPkgs pkgs = MuList $ map mkStrContext $ zipMarkLast exPkg pkgs

    zipMarkLast _ [] = []
    zipMarkLast f [p] = [f True p]
    zipMarkLast f (p:ps) = f False p : zipMarkLast f ps

    exPkg _ p "name" = MuVariable $ pad 30 $ pkgName p
    exPkg _ p "version" = MuVariable $ showVersion $ pkgVersion p
    exPkg c _ "comma" = MuVariable $ if c then "" else ","
    exPkg _ _ t = error $ "exPkg: unexpected template tag " ++ t

    pad n s = s ++ replicate (n - length s) ' '


expandBuildConfig :: BuildConfig -> Expander m -> Expander m
expandBuildConfig BuildConfig{..} defEx = ex
  where
    ex "hpVersion" = MuVariable . showVersion . hpVersion $ bcHpVersion
    ex "ghcVersion" = MuVariable . showVersion . ghcVersion $ bcGhcVersion
    ex "arch" = MuVariable bcArch
    ex t = defEx t


asContext :: (Monad m) => Expander m -> MuContext m
asContext = mkStrContext -- (return .)



type ContextMaker = Action (MuContext Action)

releaseContext :: ContextMaker
releaseContext = do
    rls <- askHpRelease
    return $ asContext $ expandRelease rls $ expandError

buildConfigContext :: ContextMaker
buildConfigContext = do
    bc <- askBuildConfig
    return $ asContext $ expandBuildConfig bc $ expandError

platformContext :: ContextMaker
platformContext =
    asContext <$> platformExpander

platformExpander :: (Monad m) => Action (Expander m)
platformExpander = do
    bc <- askBuildConfig
    rls <- askHpRelease
    return $ expandBuildConfig bc $ expandRelease rls $ expandError

templateDirname :: String
templateDirname = "templates"


copyExpandedFile :: MuContext Action -> FilePath -> FilePath -> Action ()
copyExpandedFile = copyExpanded' False

copyExpandedDir :: MuContext Action -> FilePath -> FilePath -> Action ()
copyExpandedDir = copyExpanded' True

copyExpanded' :: Bool -> MuContext Action -> FilePath -> FilePath -> Action ()
copyExpanded' isDir ctx srcTop dstTop = copyTop srcTop dstTop

  where
    copyTop = if isDir then copyTree (== templateDirname)
                       else expandFile

    copyTree skipDir src dst = do
        putLoud $ "  dir " ++ src
        makeDirectory dst

        srcFiles <- getDirectoryFiles src ["*"]
        forM_ srcFiles $ \f -> copyOne (src </> f) (dst </> f)

        srcDirs <- getDirectoryDirs src
        forM_ srcDirs $ \d -> unless (skipDir d) $
            copyTree (const False) (src </> d) (dst </> d)

    copyOne src dst = do
        putLoud $ "  file " ++ src
        case takeExtension src of
            ".mu" -> expandFile src (dropExtension dst)
            _     -> copyFile' src dst

    expandFile from to = do
        let conf = case takeExtension to of
                        ".html" -> muHtmlConf
                        _ -> muPlainConf
        need [from]
        hastacheFile conf from ctx >>= liftIO . TL.writeFile to
        liftIO $ copyPermissions from to

    muPlainConf = MuConfig
                { muEscapeFunc = emptyEscape
                , muTemplateFileDir = if isDir
                                        then Just $ srcTop </> templateDirname
                                        else Nothing
                , muTemplateFileExt = Just ".mu"
                , muTemplateRead =
                    \f -> need [f] >> muTemplateRead defaultConfig f
                }
    muHtmlConf = muPlainConf { muEscapeFunc = htmlEscape }

