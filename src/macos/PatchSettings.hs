#!/usr/bin/env runghc

module Main where

{-
    PathcSettings.hs - patch up tool settings for Mac OS X
    
    This program finds the locations of gcc and ar, which are needed by
    GHC. If they are different than in GHC's settings file, then this
    program patches it.
    
    - Mark Lentczner    
-}

import Control.Monad (unless, when)
import System.Console.GetOpt
import System.Directory (copyFile, doesFileExist, renameFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

-- | Locations where the developer command line tools might be installed.
-- In order of preference.
toolLocations :: [FilePath]
toolLocations =
    [ "/Applications/Xcode.app/Contents/Developer/usr/bin"
    , "/Developer/usr/bin"
    , "/usr/bin"
    ]

-- | Tools we need to find and possible patch their locations.
-- These are pairs where the first is the key in the settings file, and
-- the second is the base command name.
tools = [("ar command", "ar")
        ,("C compiler command", "gcc")
        ]

-- | Locate where a program is. The second argument is the current location
-- from the settings file. If present, this will be returned. Otherwise,
-- the toolLocations are probed. If program can't be found, it is an error.
locate :: String -> FilePath -> IO FilePath
locate prog curr = do
    let probes = curr : map (</> prog) toolLocations
    findFirst probes
  where
    findFirst [] = putStrLn ("Couldn't locate " ++ prog) >> exitFailure
    findFirst (p:ps) = do
        pExists <- doesFileExist p
        if pExists
            then putStrLn ("Found " ++ prog ++ " at " ++ p) >> return p
            else findFirst ps

main :: IO ()
main = do
    settingsPath <- getPath
    settings <- read `fmap` readFile settingsPath
    settings' <- mapM fixUp settings
    if settings /= settings'
        then do
                rewrite settingsPath settings'
                putStrLn (settingsPath ++ " rewritten")
        else putStrLn "No changes needed"
        
  where
    getPath = do
        args <- getArgs
        case args of
            []  -> putStrLn "Specify settings file as argument" >> exitFailure
            [f] -> return f
            _   -> putStrLn "Too many arguments" >> exitFailure
    
    fixUp e@(k,v) = case lookup k tools of
        Just prog -> locate prog v >>= \v' -> return (k,v')
        Nothing -> return e
        
    rewrite path settings = do
        let bak = path ++ ".bak"
        bakExists <- doesFileExist bak
        unless bakExists $
            renameFile path bak
        writeFile path $ (nicely . show) settings

    nicely (')':',':r) = ')':',':'\n':' ' : nicely r
    nicely (')':r)     = ')':'\n'    :' ' : nicely r
    nicely (',':r)     = ',':' '          : nicely r
    nicely (c:r)       = c                : nicely r
    nicely []          = "\n\n"

