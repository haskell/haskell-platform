{-# LANGUAGE RecordWildCards #-}

module Website where

import Development.Shake

import Dirs
import Paths
import Templates

websiteRules :: FilePath -> Rules ()
websiteRules templateSite = do
    websiteDir */> \dst -> do
        ctx <- buildConfigContext
        copyExpandedDir ctx templateSite dst
