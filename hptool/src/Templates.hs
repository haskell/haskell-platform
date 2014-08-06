{-# LANGUAGE RecordWildCards #-}

module Templates
    ( ctxEmpty, ctxAppend, ctxConcat
    , errorCtx
    , releaseContext, buildConfigContext, platformContext
    , copyExpandedFile, copyExpandedDir
    )
  where

import Control.Monad (foldM, forM_, unless)
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



ctxEmpty :: (Monad m) => MuContext m
ctxEmpty = const $ return MuNothing

ctxAppend :: (Monad m) => MuContext m -> MuContext m -> MuContext m
a `ctxAppend` b = ctxConcat [a, b]

ctxConcat :: (Monad m) => [MuContext m] -> MuContext m
ctxConcat cs t = foldM mix MuNothing cs
  where
    mix MuNothing ctx = ctx t
    mix r         _   = return r

errorCtx :: (Monad m) => MuContext m
errorCtx t = return $ MuLambda $ const msg
  where
    msg = "### unknown tag: " ++ decodeStr t ++ " ###"



releaseContext :: Action (MuContext Action)
releaseContext = askHpRelease >>= return . expandRelease

expandRelease :: (Monad m) => Release -> MuContext m
expandRelease rel = mkStrContext ex
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

    ex _ = MuNothing

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


buildConfigContext :: Action (MuContext Action)
buildConfigContext = askBuildConfig >>= return . expandBuildConfig

expandBuildConfig :: (Monad m) => BuildConfig -> MuContext m
expandBuildConfig BuildConfig{..} = mkStrContext ex
  where
    ex "hpVersion" = MuVariable . showVersion . hpVersion $ bcHpVersion
    ex "ghcVersion" = MuVariable . showVersion . ghcVersion $ bcGhcVersion
    ex "arch" = MuVariable bcArch
    ex _ = MuNothing


platformContext :: Action (MuContext Action)
platformContext = do
    rlsCtx <- releaseContext
    bcCtx <- buildConfigContext
    return $ ctxConcat [bcCtx, rlsCtx, errorCtx]


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

