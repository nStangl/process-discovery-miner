module Miner.AlphaMiner where

import Types

import qualified Data.Set as Set

import Data.Tuple (swap)
import Data.List
import Data.Bifunctor
import IOHelper.XESReader (readXES)

footprintMatrix :: EventLog -> String
footprintMatrix = undefined

ordFollowDir :: EventLog -> Set.Set (Activity, Activity)
ordFollowDir = Set.fromList . concatMap (\t -> zip t (tail t))

ordCausal :: EventLog -> Set.Set (Activity, Activity)
ordCausal l =
    let s = ordFollowDir l
    in Set.filter (\x -> Set.notMember (swap x) s) s

ordChoice :: EventLog -> Set.Set (Activity, Activity)
ordChoice l =
    let
        ele = Set.toList $ tL l
        ids = map (\x -> (x,x)) ele
        xs = Set.toList (ordFollowDir l)
    in Set.fromList $ ((allPairs ele \\ xs) \\ map swap xs) ++ ids

ordParallel :: EventLog -> Set.Set (Activity, Activity)
ordParallel l =
    let s = ordFollowDir l
    in Set.filter (\x -> Set.member (swap x) s) s

tL :: EventLog -> Set.Set Activity
tL = Set.fromList . concat

tI :: EventLog -> Set.Set Activity
tI = Set.fromList . map head

tO :: EventLog -> Set.Set Activity
tO = Set.fromList . map last

xL :: EventLog -> Set.Set (Set.Set Activity, Set.Set Activity)
xL l = Set.fromList $ map (Data.Bifunctor.bimap Set.fromList Set.fromList) xs
    where xs = xLBruteForceLists l

xLBruteForceLists :: EventLog -> [Transition]
xLBruteForceLists l = filter (uncurry (xLFilter causal choice)) (getAllPermu xs xs)
    where
        causal = Set.toList $ ordCausal l
        choice = Set.toList $ ordChoice l
        xs = Set.toList $ tL l

-- TODO: optimise two elem calls into one
xLFilter :: [(Activity, Activity)] -> [(Activity, Activity)] -> [Activity] -> [Activity] -> Bool
xLFilter causal choice as bs = cond1 as bs && cond2 as && cond2 bs
    where
        cond1 xs ys = and [(x,y) `elem` causal| x<-xs, y<-ys]
        cond2 xs = and [ elem (x1,x2) choice || elem (x1,x2) choice | (x1,x2) <- allPairs xs]


yLLists :: [([Activity], [Activity])] -> [Transition]
yLLists xs = xs \\ toRemove
    where
        subset x = filter (/= x) (getAllPermuPairs x)
        toRemove = concatMap subset xs

alphaMiner :: EventLog -> [Transition]
alphaMiner elog = start ++ transitions ++ end
    where
        start = map (\x -> (["start"], [x])) (Set.toList (tI elog))
        end = map (\x -> ([x], ["end"])) (Set.toList (tO elog))
        transitions = yLLists $ xLBruteForceLists elog


expToCytoGraph :: EventLog -> [Transition] -> CytoGraph
expToCytoGraph elog ts = CytoGraph nodes' edges'
    where
        nodes1 = CytoNode "start" "ellipse" : CytoNode "end" "ellipse" : map (`CytoNode` "rectangle") (Set.toList (tL elog))
        (startend, ts') = partition containsStartEnd ts
        edges1 = map transformStartEndEdges startend
        edges2Fs = fmap transformEdges ts' 
        (edges2, nodes2) = unzip $ zipWith (\ f i -> f i) edges2Fs [1..]
        nodes' =  nodes1 ++ nodes2
        edges'' = concat (edges1 ++ edges2)
        edges' = fmap (\(e, i) -> e { edgeID = "e" ++ show i}) (zip edges'' ([0..] :: [Integer]))

testexp :: String -> IO ()
testexp path = do
    mlog <- readXES path
    case mlog of
        Just l -> do {-return $ nodes $ expToCytoGraph l (alphaMiner l)-}
            let cytoGraph = expToCytoGraph l (alphaMiner l)
            putStrLn "CytoGraph"
            putStrLn "Nodes:"
            mapM_ print (nodes cytoGraph)
            putStrLn "\nEdges:"
            mapM_ print (edges cytoGraph)
        Nothing -> undefined

containsStartEnd :: Transition -> Bool
containsStartEnd (["start"], _) = True
containsStartEnd (_, ["end"]) = True
containsStartEnd _ = False

transformStartEndEdges :: Transition -> [CytoEdge]
transformStartEndEdges (["start"], rs) = map (\r -> CytoEdge "__" "start" r "triangle") rs
transformStartEndEdges (ls, ["end"]) = map (\l -> CytoEdge "__" l "end" "triangle") ls
transformStartEndEdges _ = undefined

transformEdges :: Transition -> Int -> ([CytoEdge], CytoNode)
transformEdges (ls, rs) i =
    let place = "p" ++ show i
    in
        (nub (concat [ [CytoEdge "__" l place "triangle" , CytoEdge "__" place r "triangle"] | l <- ls, r <- rs])
        , CytoNode place "ellipse")


{-------------------------------- General auxiliary functions ------------------------------------------}

-- calculates all possible pairs from a given list without the need of a Eq constraint
allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs [_] = []
allPairs (x:xs) = concatMap (\y -> [(x,y),(y,x)]) xs ++ allPairs xs

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

powersetNonEmpty :: [a] -> [[a]]
powersetNonEmpty [] = []
powersetNonEmpty xs = filter (not . null) (powerset xs)

getAllPermuPairs :: ([a], [a]) -> [([a], [a])]
getAllPermuPairs = uncurry getAllPermu

getAllPermu :: [a] -> [a] -> [([a], [a])]
getAllPermu xs ys = [(xs', bs') | xs' <- powersetNonEmpty xs, bs' <- powersetNonEmpty ys]

getAllSinglePermu :: [a] -> [a] -> [([a],[a])]
getAllSinglePermu xs ys = [([x],[y]) | x<-xs, y<-ys]