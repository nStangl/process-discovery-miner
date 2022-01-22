module Handler.AlphaMinerSpec (spec) where

import Prelude ()
import TestImport
import System.Directory
import Types 
import Miner.AlphaMiner
import TestData
import qualified Data.Set as Set
import IOHelper.XESReader
import Data.Bifunctor ( Bifunctor(bimap) )

-- Type alias for convinient check by Eq (equality)
-- Sets are considered equal independent of the order of their elements
type CmpTransitionList = Set.Set CmpTransition

toCmpTransitionList :: [Transition] -> CmpTransitionList
toCmpTransitionList = Set.fromList . map toCmpTransition

fromCmpTransitionList :: CmpTransitionList -> [Transition]
fromCmpTransitionList = map fromCmpTransition . Set.toList

type CmpTransition = (Set.Set Activity, Set.Set Activity)

toCmpTransition :: Transition -> CmpTransition
toCmpTransition = Data.Bifunctor.bimap Set.fromList Set.fromList

fromCmpTransition :: CmpTransition -> Transition
fromCmpTransition = Data.Bifunctor.bimap Set.toList Set.toList

spec :: Spec
spec = do
    -- Returns true if all logs from [1] are in the /test/logs/ directory
    -- [1]: https://lehre.bpm.in.tum.de/~pm-prak/
    describe "Check if all Logs are present" $ do
        it "L1-L7, running-example, flyer-,bill-,posterinst exist" $ do
            allLogsPresent `shouldReturn` True
    -- Tests if the parser yields the same EventLog's as defined in TestData
    describe "Parsing EventLog from File" $ do
        it "L1 parses correctly" $ do
            testParseEventLog "L1" getLog1
        it "L2 parses correctly" $ do
            testParseEventLog "L2" getLog2
        it "L3 parses correctly" $ do
            testParseEventLog "L3" getLog3
        it "L4 parses correctly" $ do
            testParseEventLog "L4" getLog4
        it "L5 parses correctly" $ do
            testParseEventLog "L5" getLog5
        it "L6 parses correctly" $ do
            testParseEventLog "L6" getLog6
        it "L7 parses correctly" $ do
            testParseEventLog "L7" getLog7

    -- Testing the order relations could probably be realised with QuickCheck.
    -- But I am bad with QuickCheck and implementation is trivial
    describe "Testing order relations" $ do
        describe "order follow directly" $ do
            it "L1" $ do
                testOrdRel (ordFollowDir getLog1) getLog1OrdFollowDir
            it "L2" $ do
                testOrdRel (ordFollowDir getLog2) getLog2OrdFollowDir
        describe "order causal" $ do
            it "L1" $ do
                testOrdRel (ordCausal getLog1) getLog1OrdCausal
            it "L2" $ do
                testOrdRel (ordCausal getLog2) getLog2OrdCausal
        describe "order choice" $ do
            it "L1" $ do
                testOrdRel (ordChoice getLog1) getLog1OrdChoice
            it "L2" $ do
                testOrdRel (ordChoice getLog2) getLog2OrdChoice
        describe "order parallel" $ do
            it "L1" $ do
                testOrdRel (ordParallel getLog1) getLog1OrdParallel
            it "L2" $ do
                testOrdRel (ordParallel getLog2) getLog2OrdParallel

    describe "Testing xL" $ do
        it "L1" $ do
            testxL getLog1 getLog1xL
        it "L2" $ do
            testxL getLog2 getLog2xL
        it "L3" $ do
            testxL getLog3 getLog3xL
        it "L4" $ do
            testxL getLog4 getLog4xL
        it "L5" $ do
            testxL getLog5 getLog5xL
        it "L6" $ do
            testxL getLog6 getLog6xL
        it "L7" $ do
            testxL getLog7 getLog7xL
        it "posterinst" $ do
            testxL getPoster getPosterxL
        it "billinst" $ do
            testxL getBill getBillxL 
        it "flyerinst" $ do
            testxL getFlyer getFlyerxL
        it "running-example" $ do
            testxL getRunEx getRunExxL

    describe "Testing yL" $ do
        it "L1" $ do
            testyL getLog1 getLog1yL
        it "L2" $ do
            testyL getLog2 getLog2yL
        it "L3" $ do
            testyL getLog3 getLog3yL
        it "L4" $ do
            testyL getLog4 getLog4yL
        it "L5" $ do
            testyL getLog5 getLog5yL
        it "L6" $ do
            testyL getLog6 getLog6yL
        it "L7" $ do
            testyL getLog7 getLog7yL
        it "posterinst" $ do
            testyL getPoster getPosteryL
        it "billinst" $ do
            testyL getBill getBillyL 
        it "flyerinst" $ do
            testyL getFlyer getFlyeryL
        it "running-example" $ do
            testyL getRunEx getRunExyL  
            

toFilePath :: String -> FilePath
toFilePath s = "./test/logs/" ++ s ++ ".xes"

-- | Parse EventLog and wrap in Set to check for Eq (Equality) without regard for order
testParseEventLog :: String -> EventLog -> Expectation
testParseEventLog l elog =  parsedEventLog `shouldReturn` Right (Set.fromList elog)
    where
        parsedEventLog = do
            res <- readXESFile $ toFilePath l
            return $ wrapSet res
        wrapSet :: Either String EventLog -> Either String (Set Trace)
        wrapSet e = case e of
            Left  s -> Left s
            Right r -> Right $ Set.fromList r

testOrdRel :: [(Activity,Activity)] -> [(Activity,Activity)] -> Expectation
testOrdRel is should = Set.fromList is `shouldBe` Set.fromList should

-- | Test xL implementation
testxL :: EventLog -> [Transition] -> Expectation
testxL elog tsShould = toCmpTransitionList tsIs `shouldBe` toCmpTransitionList tsShould
    where
        tsIs = xL elog
        
-- | Tests yL implementation
testyL :: EventLog -> [Transition] -> Expectation
testyL elog tsShould = tsIs `shouldBe` toCmpTransitionList tsShould
    where
        tsIs =  toCmpTransitionList $ yL $ xL elog
        
logs :: [FilePath]
logs = map toFilePath logs'
    where
        logs' = ["posterinstances","running-example","billinstances","flyerinstances"]
                ++ ["L" ++ show i | i <- [1..7] :: [Int]]

allLogsPresent :: IO Bool
allLogsPresent = foldr (&&) True <$> mapM doesFileExist logs
