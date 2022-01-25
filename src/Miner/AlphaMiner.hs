{-# LANGUAGE TupleSections #-}
module Miner.AlphaMiner where

import Types

import Data.Tuple (swap)
import Data.List ( (\\), nub, partition, sort, find, sortBy, groupBy )
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Function ( on )
import ClassyPrelude ( fromMaybe )

ordFollowDir :: EventLog -> [(Activity, Activity)]
ordFollowDir = nub . concatMap (\t -> zip t (tail t))

ordCausal :: EventLog -> [(Activity, Activity)]
ordCausal l =
    let s = ordFollowDir l
    in nub (filter ((`notElem` s) . swap) s)

ordChoice :: EventLog -> [(Activity, Activity)]
ordChoice l =
    let
        ele = tL l
        ids = map (\x -> (x,x)) ele
        xs = ordFollowDir l
    in nub $ ((allPairs ele \\ xs) \\ map swap xs) ++ ids

ordParallel :: EventLog -> [(Activity, Activity)]
ordParallel l =
    let s = ordFollowDir l
    in nub $ filter (\x -> swap x `elem` s) s

tL :: EventLog -> Trace
tL = nub . concat

tI :: EventLog -> Trace
tI = nub . map head

tO :: EventLog -> Trace
tO = nub . map last

-- | Much bruteforce, very wow
xL :: EventLog -> [Transition]
xL l = filter (uncurry (xLFilter causal choice)) (getAllPermu xs xs)
    where
        causal = ordCausal l
        choice = ordChoice l
        xs = tL l

-- | Filter condition for xL
xLFilter :: [(Activity, Activity)] -> [(Activity, Activity)] -> [Activity] -> [Activity] -> Bool
xLFilter causal choice as bs = cond1 as bs && cond2 as && cond2 bs
    where
        cond1 xs ys = and [(x,y) `elem` causal| x<-xs, y<-ys]
        cond2 xs = and [ elem (x1,x2) choice || elem (x1,x2) choice | (x1,x2) <- allPairs xs]

yL :: [Transition] -> [Transition]
yL xs = xs \\ toRemove
    where
        subset x = filter (/= x) (getAllPermuPairs x)
        toRemove = concatMap subset xs

-- | Take result of yL and add Start and end transitions
alphaMiner :: EventLog -> [Transition]
alphaMiner elog = start ++ transitions ++ end
    where
        start = map (\x -> (["start"], [x])) (tI elog)
        end = map (\x -> ([x], ["end"])) (tO elog)
        transitions = yL $ xL elog

-- | Takes the EventLog and result of the alphaMiner and returns a CytoGraph
expToCytoGraph :: EventLog -> [Transition] -> CytoGraph
expToCytoGraph elog ts = CytoGraph nodes' edges'
    where
        nodes1 = CytoNode "start" "ellipse" : CytoNode "end" "ellipse" 
                : map (`CytoNode` "rectangle") (tL elog)
        (startend, ts') = partition containsStartEnd ts
        edges1 = map transformStartEndEdges startend
        edges2Fs = fmap transformEdges ts'
        (edges2, nodes2) = unzip $ zipWith (\ f i -> f i) edges2Fs [1..]
        nodes' =  nodes1 ++ nodes2
        edges'' = concat (edges1 ++ edges2)
        edges' = fmap (\(e, i) -> e { edgeID = "e" ++ show i}) (zip edges'' ([0..] :: [Integer]))

-- | Calculates all sets used within/for the alphaminer
alphaminersets :: EventLog -> AlphaMinerSets
alphaminersets elog = do
    let xl' = xL elog
    AlphaMinerSets {
        tl= tL elog,
        ti= tI elog,
        to= tO elog,
        xl= xl' ,
        yl= yL xl'}

-- | Creates a FootprintMatrix from given EventLog
createFpMatrix :: EventLog -> FootprintMatrix
createFpMatrix elog = do
    -- Sort the Activities and define a rank/index for each Activity
    let rows = sort $ nub $ concat elog
    let indexer = zip rows ([1..] :: [Int])

    let fAddStr :: [(Int, Int)] -> String -> [((Int,Int), String)]
        fAddStr xs str = map (, str) xs

    -- Create ordering relations; then
    -- index Activity based on ordering
    let ordCaus = indexActivities indexer $ ordCausal elog
    let ordCausR = fAddStr (map swap ordCaus) "<-"
    let ordCaus' = fAddStr ordCaus "->"

    let ordChoi = ordChoice elog
    let ordChoi' = fAddStr (indexActivities indexer ordChoi) "#"

    let ordPar = ordParallel elog
    let ordPar' = fAddStr(indexActivities indexer ordPar) "||"

    let xs = concat [ordCaus', ordCausR, ordPar', ordChoi']
    -- sort and group them based on postiton tuple
    let groupFirst = sortAndGroupBy (fst .fst) xs
    let groupSecond = fmap (sortAndGroupBy (snd .fst)) groupFirst
    -- remove positions and create table
    let striped' = (map.map.map) snd groupSecond
    let combined = (map.map) unwords striped'

    FootprintMatrix (length rows) rows combined

-- | Sorts and groups by one element in the first tuple
sortAndGroupBy :: Ord a => (((a,a),b) -> a) -> [((a,a), b)]  -> [[((a,a), b)]]
sortAndGroupBy f = groupBy ((==) `on` f) . sortBy (compare `on` f)

-- | Takes a list of mappings that define ranking, i.e. ("a",1) and transforms Activity to Int
indexActivities :: [(Activity, Int)] -> [(Activity, Activity)] -> [(Int, Int)]
indexActivities ind = map (bimap find' find')
    where
        find' :: Activity -> Int
        find' a' = snd $ fromMaybe ("" :: Activity, -1) $ find (\x -> fst x == a') ind

containsStartEnd :: Transition -> Bool
containsStartEnd (["start"], _) = True
containsStartEnd (_, ["end"]) = True
containsStartEnd _ = False

-- | Transform Transitions that contain start or end to CytoEdges
transformStartEndEdges :: Transition -> [CytoEdge]
transformStartEndEdges (["start"], rs) = map (\r -> CytoEdge "__" "start" r "triangle") rs
transformStartEndEdges (ls, ["end"]) = map (\l -> CytoEdge "__" l "end" "triangle") ls
transformStartEndEdges _ = []

-- | Transform Transition to CytoEdge and their corresponding required CytoNodes
transformEdges :: Transition -> Int -> ([CytoEdge], CytoNode)
transformEdges (ls, rs) i =
    let place = "p" ++ show i
        edge f t = CytoEdge "__" f t "triangle"
    in
        (nub (concat [ [edge l place , edge place r] | l <- ls, r <- rs])
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