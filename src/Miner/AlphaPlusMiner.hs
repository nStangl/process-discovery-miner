module Miner.AlphaPlusMiner where

import Types
import Miner.AlphaMiner (ordFollowDir, ordParallel, ordChoice, tL, tI, tO, yL, getAllPermu, allPairs, containsStartEnd, transformStartEndEdges)
import Data.List (nub, intersect, (\\), group, findIndices, partition, find)
import Data.Tuple (swap)
import Control.Monad (liftM2)

type Length1Loop = (Activity,Activity,Activity)


-- | a `ordSymmentric` b <=> EventLog contains sequence ..aba..
ordSymmetric :: EventLog -> [(Activity,Activity)]
ordSymmetric = nub . concatMap getSymm
    where
        getSymm :: Trace -> [(Activity,Activity)]
        getSymm t = [ (a,b) | let n = length t - 2 - 1, i<-[0..n],
                          let a = t !! i, let b = t !! (i + 1),
                          let a' = t !! (i + 2), a == a' && b /= a ]

-- | a `ordSymmetricSwap` <=> EventLog contains ..aba.. AND ..bab..
ordSymmetricSwap :: EventLog -> [(Activity,Activity)]
ordSymmetricSwap = liftM2 intersect ordSymmetric (map swap . ordSymmetric)

-- | a `ordCausal'` b <=> a `ordFollowDir` b AND (b `not ordFollowDir` a OR a `ordSymmetricSwap` b)
ordCausal' :: EventLog -> [(Activity,Activity)]
ordCausal' l = let xs = ordFollowDir l
                   ys = ordSymmetricSwap l
    in filter (\x -> swap x `notElem` xs || x `elem` ys) xs

-- | a `ordParallel'` <=> a `ordFollowDir` b AND b `ordFollowDir` a AND a `not ordSymmetricSwap` b
ordParallel' :: EventLog -> [(Activity,Activity)]
ordParallel' l = ordParallel l \\ ordSymmetricSwap l

-- | xL for the AlphaPlusMiner
xL :: EventLog -> [Transition]
xL l = filter (uncurry (xLFilter causal choice)) (getAllPermu xs xs)
    where
        causal = ordCausal' l
        choice = ordChoice l
        xs = tL l

-- | Filter condition for xL
xLFilter :: [(Activity, Activity)] -> [(Activity, Activity)] -> [Activity] -> [Activity] -> Bool
xLFilter causal choice as bs = cond1 as bs && cond2 as && cond2 bs
    where
        cond1 xs ys = and [(x,y) `elem` causal| x<-xs, y<-ys]
        cond2 xs = and [ elem (x1,x2) choice || elem (x1,x2) choice | (x1,x2) <- allPairs xs]

-- | Take result of yL and add Start and end transitions
alphaPlusMiner :: EventLog -> ([Transition], [Length1Loop])
alphaPlusMiner elog = (start ++ transitions ++ end, l1ls)
    where
        (elog', l1ls) = preprocess elog
        start = map (\x -> (["start"], [x])) (tI elog')
        end = map (\x -> ([x], ["end"])) (tO elog')
        transitions = yL $ xL elog'

exportToCytoGraph' :: EventLog -> [Transition] -> [Length1Loop] -> CytoGraph
exportToCytoGraph' elog ts l1ls = CytoGraph nodes' edges'
    where
        nodes1 = CytoNode "start" "ellipse" : CytoNode "end" "ellipse"
                : map (`CytoNode` "rectangle") (tL elog)
        (startend, ts') = partition containsStartEnd ts
        edgesStartEnd = fmap transformStartEndEdges startend
        edgeFuncs = fmap (`transformEdges` l1ls) ts'
        (edges2, nodes2) = unzip $ zipWith (\ f i -> f i) edgeFuncs [1..]
        nodes' = nub $ nodes1 ++ concat nodes2
        edges'' = concat (edgesStartEnd ++ edges2)
        edges' = fmap (\(e, i) -> e { edgeID = "e" ++ show i}) (zip edges'' ([0..] :: [Integer]))

transformEdges :: Transition -> [Length1Loop] -> Int -> ([CytoEdge], [CytoNode])
transformEdges (ls,rs) l1ls i =
    let place = "p" ++ show i
        edge f t = CytoEdge "__" f t "triangle"
        maybFound = find (\(a,_,c) -> [a] == ls && [c] == rs) l1ls
        (es, ns) = (nub (concat [ [edge l place , edge place r] | l <- ls, r <- rs])
                        , [CytoNode place "ellipse"])
    in case maybFound of
        Nothing    -> (es, ns)
        Just (_,b,_) -> (edge place b : edge b place : es, CytoNode b "rectangle" : ns)


containsL1LAtStart :: EventLog -> Bool
containsL1LAtStart elog = or [ fst (head (groupLength trace)) > 1 | trace<-elog, length trace > 1]

containsL1LAtEnd :: EventLog -> Bool
containsL1LAtEnd elog   = or [ fst (last (groupLength trace)) > 1 | trace<-elog, length trace > 1]

groupLength :: Eq a =>  [a] -> [(Int, a)]
groupLength = map (\x -> (length x, head x)) . group

preprocess :: EventLog -> (EventLog, [Length1Loop])
preprocess elog = (elog', l1lWNeigh)
    where
        elog' = fmap (`removeL1LFromTrace` l1lWNeigh) elog
        l1lWNeigh = filterL1L elog $ nub $ concatMap findL1LWithNeighbours elog

removeL1LFromTrace :: Trace -> [Length1Loop] -> Trace
removeL1LFromTrace t xs
    | length (groupLength t) < 3 = t
    | otherwise = map snd $ removeRec (groupLength t) xs

-- | Recursively walk through grouped List, and filter all middle elements where the triple matches
removeRec :: [(Int,Activity)] -> [(Activity,Activity,Activity)] -> [(Int,Activity)]
removeRec [] _ = []
removeRec [x] _ = [x]
removeRec [x,y] _ = [x,y]
removeRec (x : y : z : xs) ys
    -- The middle element is to be filtered
    | or [ (snd x, snd y, snd z) == (a,b,c) | (a,b,c)<-ys]
    -- Remove the element in the middle and descend two elements in list
        = x : removeRec (z : xs) ys
    -- Nothing to remove, shift left and try again
    | otherwise = x : removeRec (y : z : xs) ys

filterL1L :: EventLog -> [Length1Loop] -> [Length1Loop]
filterL1L elog xs = filter cond xs'
    where
        xs' = filter (\(a,_,c) -> a /= c) xs
        cond (a,_,c) = or [ a == a' && c == c' | trace<-elog, i<-[0..length trace - 2],
                                   let (a',c') = (trace !! i, trace !! (i+1)) ]


-- | Filter loop and neighbour triple
filterL1LWithNeigh :: Trace ->  [(Activity,Activity,Activity)] -> [(Activity,Activity,Activity)]
filterL1LWithNeigh _ = filter (\(a,_,c) -> a /= c)

findL1LWithNeighbours :: Trace -> [(Activity,Activity,Activity)]
findL1LWithNeighbours t
    | length t < 4 = []
    | otherwise = let grouped = groupLength t
                      is = findIndices (\x -> fst x > 1) grouped
                      n = length grouped
                      xs = [ (snd (grouped !! (i-1)), snd (grouped !! i), snd (grouped !! (i+1))) | i<-is, i /= 0, i <= n - 2]
                  in xs

postprocess :: [Transition] -> [Transition]
postprocess = undefined

testTraceL6 :: EventLog
testTraceL6 = [["a","c","e","g"],["a","e","c","g"],["b","d","f","g"],["b","f","d","g"]]

-- L1L example
testTraceL7 :: EventLog
testTraceL7 = [["a","c"],["a","b","c"],["a","b","b","c"],["a","b","b","b","b","c"]]

-- L2L example
testTraceL8 :: EventLog
testTraceL8 = [["a","b","d"], ["a","b","c","b","d"],["a","b","c","b","c","b","d"]]

testTraceL9 :: EventLog
testTraceL9 = [["a","c","d"],["b","c","e"]]