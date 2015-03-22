{-# LANGUAGE RecordWildCards #-}

module Templates
    ( ctxEmpty, ctxAppend, ctxConcat
    , errorCtx
    , mapListContext, mapListStrContext
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


-- | Create a `MuList` by mapping a context creating function over a list.
-- In addition, the context will be augmented to support tags "first" and "last"
-- that are `MuBool` values, and "index" which is a `MuVariable` `Int`.
mapListContext :: (Monad m) => (a -> MuContext m) -> [a] -> MuType m
mapListContext fCtx vs = MuList . map ctx' . zip [1..] $ vs
  where
    ctx' (i,v) t = do
        r <- fCtx v t
        case r of
            MuNothing -> (mkStrContext $ lCtx i) t
            _         -> return r

    n = length vs

    lCtx :: Int -> String -> MuType m
    lCtx i "first" = MuBool $ i == 1
    lCtx i "last"  = MuBool $ i == n
    lCtx i "index" = MuVariable i
    lCtx _ _ = MuNothing

mapListStrContext :: (Monad m) => (a -> String -> MuType m) -> [a] -> MuType m
mapListStrContext fCtx = mapListContext (mkStrContext . fCtx)


releaseContext :: Action (MuContext Action)
releaseContext = askHpRelease >>= return . expandRelease

expandRelease :: (Monad m) => Release -> MuContext m
expandRelease rel = mkStrContext ex
  where
    ex "hpVersion" = MuVariable . showVersion . hpVersion . relVersion $ rel
    ex "ghcVersion" = case pkgsThat [isGhc, not . isLib, not . isTool] of
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

    exPkgs = mapListStrContext exPkg

    exPkg p "name" = MuVariable $ pad 30 $ pkgName p
    exPkg p "version" = MuVariable $ showVersion $ pkgVersion p
    exPkg _ _ = MuNothing

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

