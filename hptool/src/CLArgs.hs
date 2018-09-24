{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Command Line Args handling.

module CLArgs
       ( checkAndValidateArgs
       , flags
       , Flags(..)
       , ValidationErrs
       )
  where

import Control.Monad.Trans.Writer.Strict ( execWriterT )
import System.Console.GetOpt

import Types

import Internal.CLArgs


-- | Long Options, provided at top-level so they can be used in the
-- specific error messages (the OptDescr is not very ammenable to mapping
-- from a value in Flags to the option).  Note no short options are used
-- since otherwise they likely would conflict with the Shake command-line
-- options, so we use a long option, with an "X" before our "short" option.
--  --Xb: cabal executable file
--  --Xc: core build only
--  --Xf: full build
--  --Xg: ghc-bindist (tar file)
--  --Xh: haddock docs in HTML (tar file)
--  --Xi: print info
--  --Xl: ghc libraries docs in HTML (tar file)
--  --Xp: ghc user's guide PDF (file)
--  --Xs: stack installer or executable file
--  --Xu: ghc user's guide in HTML (tar file)
--  --Xx: set installation prefix (for Posix builds)
optCabalExe, optCore, optFull, optGHCBinDist, optHaddockDocs, optInfo,
  optGHCLibsHTML, optGHCUsersPDF, optStackExe, optGHCUsersHTML, optPrefix,
  optUsage :: [Char]
optCabalExe     = "Xb"
optCore         = "Xc"
optFull         = "Xf"
optGHCBinDist   = "Xg"
optHaddockDocs  = "Xh"
optInfo         = "Xi"
optGHCLibsHTML  = "Xl"
optGHCUsersPDF  = "Xp"
optStackExe     = "Xs"
optGHCUsersHTML = "Xu"
optPrefix       = "Xx"
optUsage        = "X?"

-- If we use the long option of "help", shake intercepts that from us.
flags :: [OptDescr (Either a Flags)]
flags = [ Option "" [optUsage] (NoArg $ Right Usage)
                     "Show command line flags and options."
        , Option "" [optCabalExe] (ReqArg (Right . CabalExe) "PATH")
                     "Path to the cabal executable to use during build."
        , Option "" [optCore] (NoArg (Right $ FlavorFlag BuildFlavorCore))
                     "Perform a core-only HP build."
        , Option "" [optFull] (NoArg $ Right $ FlavorFlag BuildFlavorFull)
                     "Perform a full HP build."
        , Option "" [optGHCBinDist] (ReqArg (Right . GHCBinDist) "TAR")
                     "Path to the GHC distribution tarfile."
        , Option "" [optHaddockDocs] (ReqArg (Right . HaddockHTML) "TAR")
                     "WINDOWS: Path to Haddock documentation tarfile."
        , Option "" [optInfo] (NoArg $ Right Info)
                     "Show details on packages are in this HP release."
        , Option "" [optGHCLibsHTML] (ReqArg (Right . GHCLibsHTML) "TAR")
                     "WINDOWS: Path to GHC library docs tarfile."
        , Option "" [optGHCUsersPDF] (ReqArg (Right . GHCUsersPDF) "PDF")
                     "WINDOWS: Path to the GHC User's Guide (PDF file)."
        , Option "" [optStackExe] (ReqArg (Right . StackExe) "PATH")
                     "Path to the stack installer or executable."
        , Option "" [optGHCUsersHTML] (ReqArg (Right . GHCUsersHTML) "TAR")
                     "WINDOWS: Path to GHC User's Guide (HTML) tarfile."
        , Option "" [optPrefix] (ReqArg (Right . Prefix) "DIR")
                     "Set a custom install prefix (only Posix builds)."
        ]

flagsToConfig :: UserConfig -> Flags -> UserConfig
flagsToConfig uc                 Info            = uc
flagsToConfig uc@UserConfig{..} (Prefix a)       = uc{ ucPrefix = Just a }
flagsToConfig uc@UserConfig{..} (FlavorFlag a)   = uc{ ucBuildFlavor = a }
flagsToConfig uc@UserConfig{..} (CabalExe a)     = uc{ ucCabalExe = a }
flagsToConfig uc@UserConfig{..} (GHCBinDist a)   = uc{ ucGHCBinDist = a }
flagsToConfig uc@UserConfig{..} (StackExe a)     = uc{ ucStackExe = a }
flagsToConfig uc@UserConfig{..} (GHCUsersPDF a)  = uc{ ucGHCUsersPDF = a }
flagsToConfig uc@UserConfig{..} (GHCUsersHTML a) = uc{ ucGHCUsersHTML = a }
flagsToConfig uc@UserConfig{..} (GHCLibsHTML a)  = uc{ ucGHCLibsHTML = a }
flagsToConfig uc@UserConfig{..} (HaddockHTML a)  = uc{ ucHaddockHTML = a }
flagsToConfig uc                 Usage           = uc

defUserConfig :: UserConfig
defUserConfig = UserConfig Nothing BuildFlavorCore "" "" "" "" "" "" ""

checkAndValidateArgs :: (Monad m) => Bool -> (FilePath -> m Bool) -> [Flags]
                        -> m (UserConfig, ValidationErrs)
checkAndValidateArgs buildWin doesFileExist flgVals = do
  let checkRequiredAndExists' = checkRequiredAndExists doesFileExist flgVals
      uc = defUserConfig
      -- Some flags are required only for Windows, and are misleading if
      -- otherwise provided.
      validator =
          if buildWin then checkRequiredAndExists' else checkExcess flgVals
  validationErrors <- execWriterT $ do
    checkCoreOrFull flgVals
    checkRequiredAndExists'
      (\case (CabalExe s) -> Just s
             _            -> Nothing )
      "cabal executable" optCabalExe
    checkRequiredAndExists'
      (\case (GHCBinDist s) -> Just s
             _              -> Nothing )
      "GHC distro tarfile" optGHCBinDist
    checkRequiredAndExists'
      (\case (StackExe s) -> Just s
             _            -> Nothing )
      "Stack installer/executable" optStackExe
    validator (\case (GHCUsersPDF s) -> Just s
                     _               -> Nothing )
      "GHC User's Guide (PDF file)" optGHCUsersPDF
    validator (\case (GHCUsersHTML s) -> Just s
                     _                -> Nothing )
      "GHC User's Guide (HTML tarfile)" optGHCUsersHTML
    validator (\case (GHCLibsHTML s) -> Just s
                     _               -> Nothing )
      "Required: path to Libraries Doc HTML tarfile" optGHCLibsHTML
    validator (\case (HaddockHTML s) -> Just s
                     _               -> Nothing )
      "Required: path to Haddock HTML tarfile" optHaddockDocs
  let userConfigValidated =
          if (not $ null validationErrors )
            then uc
            else foldl flagsToConfig uc flgVals
  return ( userConfigValidated, validationErrors )
