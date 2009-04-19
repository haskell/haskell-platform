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
    [cabalpath,outdir] <- getArgs

    pwd <- getCurrentDirectory
    setCurrentDirectory outdir

    let cabal = pwd </> cabalpath


    cabalsrc  <- readPackageDescription normal cabal
    let final =  flattenPackageDescription cabalsrc
        dependencies  = buildDepends final ++
                concatMap buildTools (allBuildInfo final)

    -- relies on the fact that meta cabal package is a simple list of
    -- dependencies and versions. nothing suss.
    --
    let urls =
            [ (d, display name ++ "-" ++ display vers)
            | d@(Dependency name (ThisVersion vers)) <- dependencies ]

    forM_ urls $ \(dep@(Dependency name vers), package) -> do
        system $ "cabal unpack " ++ package


    --
    -- Too sleepy. Future: solve this via the cabal library
    --
    system $ "cabal install --dry-run --reinstall " ++ (
                        intercalate " " [ package
                                | (_, package) <- urls
                    ] ) ++ " > platform.packages.raw"
    src <- readFile "platform.packages.raw"

    let programs = drop 2 $ lines src
    let ls' = case partition ("happy" `isPrefixOf`) programs of ([h],rest) -> h : rest
    let ls = case partition ("mtl" `isPrefixOf`) ls' of ([h],rest) -> h : rest

    writeFile "platform.packages" (unlines ls) -- move happy to top of list.

    removeFile "platform.packages.raw"
    setCurrentDirectory pwd

------------------------------------------------------------------------

{-
        e <- openLazyURI url
        case e of
            Left err -> error $ "Unable to download " ++ show url ++ ": " ++  show err
            Right s  -> do createDirectory name
                           createDirectory (name </> display vers)
                           BS.writeFile (name </> display vers </> 
                                (name ++ "-" ++ display vers ++ ".tar.gz")) s
-}

{-
    home <- getHomeDirectory
    let index = home </> ".cabal/packages/hackage.haskell.org"
    x <- readRepoIndex index
    print x
-}


--    lookupDependency :: Package pkg => PackageIndex pkg -> Dependency

    -- -> [pkg]

------------------------------------------------------------------------
-- Stolen from cabal-install

{-
-- | Read a repository index from disk, from the local file specified by
-- the 'Repo'.
--
readRepoIndex :: FilePath -> IO (PackageIndex AvailablePackage)
readRepoIndex path =
    handleNotFound $ do
        let indexFile = repoLocalDir path </> "00-index.tar"
        pkgs <- either fail return . parseRepoIndex =<< BS.readFile indexFile
        C.evaluate (PackageIndex.fromList pkgs)

  where
    -- | Parse a repository index file from a 'ByteString'.
    --
    -- All the 'AvailablePackage's are marked as having come from the given 'Repo'.
    --
    parseRepoIndex :: BS.ByteString -> Either String [AvailablePackage]
    parseRepoIndex = either Left (Right . catMaybes . map extractPkg)
                   . check [] . Tar.read

    check _  (Tar.Fail err)  = Left  err
    check ok Tar.Done        = Right ok
    check ok (Tar.Next e es) = check (e:ok) es

    extractPkg :: Tar.Entry -> Maybe AvailablePackage
    extractPkg entry
      | takeExtension fileName == ".cabal"
      = case splitDirectories (normalise fileName) of
          [pkgname,vers,_] -> case simpleParse vers of
            Just ver -> Just AvailablePackage {
                packageInfoId      = PackageIdentifier ({-PackageName-} pkgname) ver,
                packageDescription = descr,
                packageSource      = RepoTarballPackage path
              }
            _ -> Nothing
            where
              parsed = parsePackageDescription . fromUTF8 . BS.Char8.unpack
                                               . Tar.fileContent $ entry
              descr  = case parsed of
                ParseOk _ d -> d
                _           -> error $ "Couldn't read cabal file "
                                    ++ show fileName
          _ -> Nothing
      | otherwise = Nothing
      where
        fileName = Tar.fileName entry

    handleNotFound action = catch action $ \e -> if isDoesNotExistError e
      then return mempty
      else ioError e

-}
