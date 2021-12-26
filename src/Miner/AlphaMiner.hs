module Miner.AlphaMiner where

import Types

import qualified Data.Set as Set

import Data.Tuple (swap)
import Data.List
import Data.Bifunctor

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

{- Used for generating JSON -}
{-
expNodesEdges :: EventLog -> [Transition] -> ([Node], [Edge])
expNodesEdges elog xs = (nodes, edges)
    where
        nodes = Node {nodeID="start", shape="ellipse"} : Node {nodeID="end", shape="ellipse"} : map (\x -> Node {nodeID = x, shape = "rectangle"} ) (Set.toList (tL elog))
        edges = undefined
        edges2 = undefined

auxexp :: Transition -> [Int] -> [Edge]
auxexp (["start"], ["end"]) i = [Edge {edgeID="p" ++ show (head i), source="start", target="end", arrow="triangle"}]
auxexp (["start"], rs) i = map (\x -> Edge {edgeID="p" ++ show (head i), source="start", target=x, arrow="triangle"}) rs
auxexp (ls, ["end"]) i = undefined
auxexp (ls, rs) i = undefined 
-}

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