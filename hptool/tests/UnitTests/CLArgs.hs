{-# LANGUAGE LambdaCase #-}

module UnitTests.CLArgs
    ( tests
    )
    where

import Control.Monad.Trans.Writer.Strict ( execWriterT )
import Data.List ( delete )
import Test.Tasty
import Test.Tasty.Hspec

import CLArgs
import Internal.CLArgs
import Types


doesFileExistMock :: (Monad m) => FilePath -> m Bool
doesFileExistMock "./exists"    = return True
doesFileExistMock _             = return False

checkRequiredAndExistsTestSimple :: [Flags] -> IO ValidationErrs
checkRequiredAndExistsTestSimple flgVals =
  execWriterT $ do
      checkRequiredAndExists doesFileExistMock flgVals
        (\case (GHCBinDist s) -> Just s
               _              -> Nothing )
        "GHC distro tarfile" ['x']

checkRequiredAndExistsTestArgExists :: IO ValidationErrs
checkRequiredAndExistsTestArgExists = checkRequiredAndExistsTestSimple flgVals
    where flgVals = [ GHCBinDist "./exists" ]

checkRequiredAndExistsTestNoArg :: IO ValidationErrs
checkRequiredAndExistsTestNoArg = checkRequiredAndExistsTestSimple flgVals
    where flgVals = []

additionalRequiredForWin :: [Flags]
additionalRequiredForWin = [ GHCUsersHTML "./exists"
                           , GHCLibsHTML "./exists"
                           , HaddockHTML "./exists"
                           , GHCUsersPDF "./exists"
                           ]

allRequiredFlagsCommon :: [Flags]
allRequiredFlagsCommon = [ CabalExe "./exists"
                         , GHCBinDist "./exists"
                         , StackExe "./exists"
                         , FlavorFlag BuildFlavorFull
                         ]

allRequiredFlagsWin :: [Flags]
allRequiredFlagsWin = allRequiredFlagsCommon ++ additionalRequiredForWin

checkAndValidateArgsTestSimple' :: Bool -> [Flags] -> IO ValidationErrs
checkAndValidateArgsTestSimple' buildWin flgVals = do
  (_, v) <- checkAndValidateArgs buildWin doesFileExistMock flgVals
  return v

checkAndValidateArgsTestSimple :: [Flags] -> IO ValidationErrs
checkAndValidateArgsTestSimple = checkAndValidateArgsTestSimple' True

checkAndValidateArgsTestAllExistsWin :: IO ValidationErrs
checkAndValidateArgsTestAllExistsWin =
    checkAndValidateArgsTestSimple allRequiredFlagsWin

checkAndValidateArgsTestAllExistsNonWin :: IO ValidationErrs
checkAndValidateArgsTestAllExistsNonWin =
    checkAndValidateArgsTestSimple' False allRequiredFlagsCommon

checkAndValidateArgsTestOneMissingFlag :: IO ValidationErrs
checkAndValidateArgsTestOneMissingFlag = checkAndValidateArgsTestSimple flgVals
    where flgVals = delete (GHCBinDist "./exists") allRequiredFlagsWin

checkAndValidateArgsTestOneMissingFile :: IO ValidationErrs
checkAndValidateArgsTestOneMissingFile = checkAndValidateArgsTestSimple flgVals
    where flgVals = (GHCBinDist "") :
                    delete (GHCBinDist "./exists") allRequiredFlagsWin

checkAndValidateArgsTestBothFC :: IO ValidationErrs
checkAndValidateArgsTestBothFC = checkAndValidateArgsTestSimple flgVals
    where flgVals = (FlavorFlag BuildFlavorCore) : allRequiredFlagsWin

checkAndValidateArgsTestNeitherFC :: IO ValidationErrs
checkAndValidateArgsTestNeitherFC = checkAndValidateArgsTestSimple flgVals
    where flgVals = delete (FlavorFlag BuildFlavorFull) allRequiredFlagsWin

checkAndValidateArgsTestExcess :: IO ValidationErrs
checkAndValidateArgsTestExcess = checkAndValidateArgsTestSimple' False flgVals
    where flgVals = allRequiredFlagsWin

checkAndValidateArgsTestAllMissingWin :: IO ValidationErrs
checkAndValidateArgsTestAllMissingWin = checkAndValidateArgsTestSimple []

checkAndValidateArgsTestAdditionalMissingWin :: IO ValidationErrs
checkAndValidateArgsTestAdditionalMissingWin =
    checkAndValidateArgsTestSimple allRequiredFlagsCommon

checkAndValidateArgsTestAllMissingNonWin :: IO ValidationErrs
checkAndValidateArgsTestAllMissingNonWin =
    checkAndValidateArgsTestSimple' False []

checkCoreOrFullTest :: [Flags] -> IO ValidationErrs
checkCoreOrFullTest = execWriterT . checkCoreOrFull

tests :: IO TestTree
tests = do
    hs <- hspecTests
    return $ testGroup "CLArgs unit tests" [hs]

hspecTests :: IO TestTree
hspecTests = do
  testSpec "CLArgs HSpec tests" $ do
    describe "checkAndValidateArgs" $ do
      it "succeeds given a Flag and the file exists" $
        checkRequiredAndExistsTestArgExists >>= (`shouldSatisfy` null)
      it "fails given a missing Flag" $
        checkRequiredAndExistsTestNoArg >>= (`shouldSatisfy` (not . null))
    describe "checkAndValidateArgs" $ do
      it "succeeds given a list of all required Flags (Windows)" $
        checkAndValidateArgsTestAllExistsWin >>= (`shouldSatisfy` null)
      it "succeeds given a list of all required Flags (non-Windows)" $
        checkAndValidateArgsTestAllExistsNonWin >>= (`shouldSatisfy` null)
      it "fails given a full list, less one missing Flag" $
        checkAndValidateArgsTestOneMissingFlag >>=
          (`shouldSatisfy` (not . null))
      it "fails given a full list, but one file doesn't exist" $
        checkAndValidateArgsTestOneMissingFile >>=
          (`shouldSatisfy` (not . null))
      it "fails if given both -f and -c" $
        checkAndValidateArgsTestBothFC >>= (`shouldSatisfy` (not . null))
      it "fails if given neither -f nor -c" $
        checkAndValidateArgsTestNeitherFC >>= (`shouldSatisfy` (not . null))
      it "fails given an arg only for Windows on non-Windows build" $
        checkAndValidateArgsTestExcess >>= (`shouldSatisfy` (not . null))
      it ("fails with " ++ (show . length $ allRequiredFlagsWin)
          ++ " errors when given no args, on Windows") $
        checkAndValidateArgsTestAllMissingWin >>=
          (\x -> (length allRequiredFlagsWin) `shouldBe` (length x))
      it ("fails with " ++ (show . length $ additionalRequiredForWin)
          ++ " errors if missing additionals for Windows") $
        checkAndValidateArgsTestAdditionalMissingWin >>=
          (\x -> (length additionalRequiredForWin) `shouldBe` (length x))
      it ("fails with " ++ (show . length $ allRequiredFlagsCommon)
          ++ " errors when given no args, on non-Windows") $
        checkAndValidateArgsTestAllMissingNonWin >>=
          (\x -> (length allRequiredFlagsCommon) `shouldBe` (length x))
    describe "checkCoreOrFull" $ do
      it "fails given both -f and -c" $
        checkCoreOrFullTest
          [FlavorFlag BuildFlavorFull, FlavorFlag BuildFlavorCore]
        >>= (`shouldSatisfy` (not . null))
      it "fails given neither -f nor -c" $
        checkCoreOrFullTest [] >>= (`shouldSatisfy` (not . null))
      it "succeeds given only -f" $
        checkCoreOrFullTest [FlavorFlag BuildFlavorFull]
        >>= (`shouldSatisfy` null)
      it "succeeds given only -c" $
        checkCoreOrFullTest [FlavorFlag BuildFlavorCore]
        >>= (`shouldSatisfy` null)
