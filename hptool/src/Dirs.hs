module Dirs
    ( (*/>), dir )
 where

import Development.Shake
import Development.Shake.FilePath

import Paths (markerRoot)
import Utils


-- | Define a directory pattern rule.
-- This defines a rule for how to build a directory of things. The supplied
-- function is passed the directory, which will be empty, and should build it
-- afresh. Since a directory can't be a dependency in Shake, a marker file is
-- built with a directory list of the directory. In order to depend on a
-- directory built with this rule, use 'needDir'
(*/>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
patDir */> act =
    patMarker *> \outMarker -> do
        let outDir = markerToDir outMarker
        makeDirectory $ takeDirectory outDir
        command_ [] "rm" [ "-Rf", "--", outDir]
        act outDir
        exists <- doesDirectoryExist outDir
        xs <- if exists
                    then getDirectoryFiles "" [outDir ++ "//*"]
                    else return []
        need xs
        writeFileChanged outMarker $ unlines xs
  where
    patMarker = dirToMarker patDir

infix 1 */>

-- | Use this in calls to need on paths that are directories.
dir :: FilePath -> FilePath
dir = dirToMarker


-- Marker files for dirs are kept off to the side, under the buildRoot.
-- This keeps the built artifacts tidy.
--
-- It is important that these functions work for both FilePath and FilePattern.
-- The transformation must be lossless and reversable. Most importantly, a
-- FilePath matching a marker pattern, must transform into a directory that
-- would have been matched by the corresponding directory pattern.
--
-- FIXME(mzero): On Windows, this code won't work for paths with drive
-- specifications (absolute and relative).

dirToMarker :: String -> String
dirToMarker d | isAbsolute d = markerRoot </> "abs" ++ d
              | otherwise = markerRoot </> "rel" </> d

markerToDir :: String -> String
markerToDir m = case strip markerRoot m of
    ("abs", d@('/':_)) -> d
    ("rel", ('/':d)) -> d
    _ -> badMarker
  where
    strip "" ('/':ds)            = break (=='/') ds
    strip (p:ps) (d:ds) | p == d = strip ps ds
    strip _ _                    = badMarker
    badMarker = error $ "markerToDir: bad marker path " ++ m

