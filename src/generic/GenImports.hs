{-
-- Given a .cabal file with the platform specification, generate a tree in
-- hackage format, with a 00-index.tgz file, so that cabal-install will be
-- able to treat the local tree as a db.
-}

import Data.List hiding (intercalate)
import System.Directory
import Control.Monad
import System.IO
import System.FilePath
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Control.Exception as C
import System.IO.Error (isDoesNotExistError)
import Data.Monoid
import Data.Maybe
import System.Environment

-- cabal-install
-- import qualified Tar as Tar
-- import Types

import Distribution.Package ( PackageIdentifier(..), Package(..) , Dependency(Dependency) )
import Distribution.PackageDescription.Parse
import Distribution.Version
import Distribution.Simple.Utils
import Distribution.Simple.PackageIndex
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription hiding (packageDescription)
import Distribution.Verbosity
import Distribution.Text ( display, simpleParse )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import System.Cmd
import System.FilePath

-- import Network.Curl.Download.Lazy

main = do
    [cabalpath] <- getArgs
    pwd <- getCurrentDirectory

    let cabal = pwd </> cabalpath

    cabalsrc  <- readPackageDescription normal cabal
    let final =  flattenPackageDescription cabalsrc
        dependencies  = buildDepends final ++
                concatMap buildTools (allBuildInfo final)

    -- relies on the fact that meta cabal package is a simple list of
    -- dependencies and versions. nothing suss.
    --
    let urls =
            [ (d, display name ++ "/" ++ display vers)
            | d@(Dependency name (ThisVersion vers)) <- dependencies ]

    forM_ urls $ \(dep@(Dependency name vers), package) -> do
        system $ "lscabal " ++
            "http://hackage.haskell.org/packages/archive/" ++ package ++ "/" ++ display name ++ ".cabal"

-- http://hackage.haskell.org/packages/archive/suffixarray/0.0.1/suffixarray.cabal

    setCurrentDirectory pwd

