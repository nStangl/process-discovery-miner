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
import Data.List (nub)
import qualified Miner.AlphaPlusMiner as AP

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
    
    describe "Testing Alpha+ Miner" $ do
        describe "New order relations" $ do
            it "x" $ do
                True `shouldBe` True
        describe "Detect loops in invalid places" $ do
            it "detect loop at start" $ do
                AP.containsL1LAtStart [["a","b"],["a","a","b"]] `shouldBe` True
            it "detect loop at end" $ do
                AP.containsL1LAtEnd [["a","b","c"],["a","b","c","c"]] `shouldBe` True
        describe "Test loops of length 1" $ do
            it "Test filtering of possible loops 1" $ do
                AP.filterL1L [["a","b","c"], ["a","b","b","c"]] [("a","b","c")] 
                `shouldBe` []
            it "Test filtering of possible loops 2" $ do
                AP.filterL1L [["a","a"], ["a","b","a"], ["a","b","b","a"]] [("a","b","a")] 
                `shouldBe` []
            
            it "Removing Loops from Trace 1" $ do
                AP.removeL1LFromTrace ["a","b","c"] [("a","b","c")]
                `shouldBe` ["a","c"]
            it "Removing Loops from Trace 2" $ do
                AP.removeL1LFromTrace ["a","b"] [("a","b","c")]
                `shouldBe` ["a","b"]
            it "Removing Loops from Trace 3" $ do
                AP.removeL1LFromTrace ["a","b"] [("a","b","c"), ("a","b","d")]
                `shouldBe` ["a","b"]

            it "L7 - EventLog" $ do
                fst (AP.preprocess getLog7) `shouldBe` getLog7Preprocessed 
            it "L7 - Found loops" $ do
                snd (AP.preprocess getLog7) `shouldBe` getLog7L1LWithNeighbours
            
            -- With L8 there should be no loops of length 1
            it "L8 - EventLog" $ do
                fst (AP.preprocess getLog8) `shouldBe` getLog8Preprocessed
            it "L8 - Found no loops" $ do
                snd (AP.preprocess getLog8) `shouldBe` []
        
            -- Postprocessing can only be Tested on the CytoGraph
            it "Test postprocessing - contains loop node" $ do
                log7PreprocessNode `shouldBe` True
            it "Test postprocessing - contains loop transitions" $ do
                log7PreprocessEdges `shouldBe` True

        describe "Test Loops of length 2" $ do
            describe "Testing new order relations" $ do
                it "New order relations - ordSymmetric 1" $ do
                    AP.ordSymmetric [["a","b","a"]] `shouldBe` [("a","b")]
                it "New order relations - ordSymmetric 2" $ do
                    AP.ordSymmetric [["a","b","a","b"]] `shouldBe` [("a","b"), ("b","a")]
                it "New order relations - ordSymmetricSwap 1" $ do         
                    AP.ordSymmetricSwap [["a","b","a","b"]] 
                    `shouldBe` 
                    nub (AP.ordSymmetric [["a","b","a","b"]])

            it "Solve Log 8" $ do
                fst (AP.alphaPlusMiner getLog8) `shouldBe` getLog8AlphaPlusResult


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

log7PreprocessEdges :: Bool
log7PreprocessEdges = and [ s `elem` cytoedges'| s<-shouldBeEdges]
    where
        (ts, l1ls) = AP.alphaPlusMiner getLog7
        cytoedges = edges $ AP.exportToCytoGraph' getLog7 ts l1ls
        cytoedges' = [ (source e, target e) | e<-cytoedges ]
        shouldBeEdges = map (\x -> (source x, target x)) $ snd getLog7PostProcessCytoElem
        
log7PreprocessNode :: Bool
log7PreprocessNode = containsNode
    where
        (ts, l1ls) = AP.alphaPlusMiner getLog7
        graph = AP.exportToCytoGraph' getLog7 ts l1ls
        containsNode = fst getLog7PostProcessCytoElem `elem` nodes graph
