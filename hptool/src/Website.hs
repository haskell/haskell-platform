{-# LANGUAGE RecordWildCards #-}

module Website where

import Development.Shake

import Dirs
import Paths
import Templates

websiteRules :: FilePath -> Rules ()
websiteRules templateSite = do
    websiteDir */> \dst -> do
        bcCtx <- buildConfigContext
        let ctx = bcCtx `ctxAppend` errorCtx
        copyExpandedDir ctx templateSite dst
