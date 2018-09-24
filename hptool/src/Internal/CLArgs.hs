{-# LANGUAGE RecordWildCards #-}

-- | An internal module used by the Command Line Args handling, split out
-- to facilitate the unit tests.

module Internal.CLArgs
       ( checkCoreOrFull
       , checkRequiredAndExists
       , checkExcess
       , Flags(..)
       , ValidationErrs
       )
  where

import Control.Monad ( unless, when )
import Control.Monad.Trans.Writer.Strict ( tell, WriterT )
import Control.Monad.Trans.Class ( lift )
import Data.List ( find )
import Data.Maybe ( fromJust, isJust )

import Types


data Flags = Info | Usage
           | Prefix String
           | FlavorFlag BuildFlavor
           | CabalExe String | GHCBinDist String | StackExe String
           | GHCUsersPDF String | GHCUsersHTML String | GHCLibsHTML String
           | HaddockHTML String
  deriving (Eq)

type ValidationErrs = [String]

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM mp m = mp >>= \p -> unless p m

checkRequiredAndExists :: (Monad m, Foldable t) =>
                          (FilePath -> m Bool) -> t a -> (a -> Maybe FilePath)
                          -> String -> [Char]
                          -> WriterT ValidationErrs m ()
checkRequiredAndExists doesFileExist flgVals maybeIsFlag flagDesc longOpt = do
  case (find (isJust . maybeIsFlag) flgVals) of
    Just a -> do
      let fname = fromJust $ maybeIsFlag a
      unlessM (lift $ doesFileExist fname) $
          fileDoesNotExistError flagDesc fname
    Nothing -> missingArgError flagDesc longOpt

fileDoesNotExistError :: (Monad m) => String -> FilePath
                         -> WriterT ValidationErrs m ()
fileDoesNotExistError flagDesc fn =
  tell [ flagDesc ++ ": File does not exist: \"" ++ fn ++ "\"." ]

missingArgError :: (Monad m) => String -> String -> WriterT ValidationErrs m ()
missingArgError flagDesc longOpt =
  tell [ "Required: path to " ++ flagDesc ++ ". Use --" ++ longOpt ++ "." ]

checkCoreOrFull :: (Monad m) => [Flags] -> WriterT ValidationErrs m ()
checkCoreOrFull flgVals = do
  let isFull = isJust $ find ((==) (FlavorFlag BuildFlavorFull)) flgVals
      isCore = isJust $ find ((==) (FlavorFlag BuildFlavorCore)) flgVals
  when (isFull == isCore) $ tell [ "Must specify exactly one of --Xf or --Xc." ]

excessArgError :: (Monad m) => String -> [Char] -> WriterT ValidationErrs m ()
excessArgError flagDesc longOpt =
  tell [ "Unused flag: " ++ flagDesc ++ ". This platform does not use -"
         ++ longOpt ++ "."]

checkExcess :: (Monad m, Foldable t) =>
               t a -> (a -> Maybe FilePath) -> String -> [Char]
               -> WriterT ValidationErrs m ()
checkExcess flgVals maybeIsFlag flagDesc longOpt = do
  case (find (isJust . maybeIsFlag) flgVals) of
    Just _ -> excessArgError flagDesc longOpt
    _      -> return ()
