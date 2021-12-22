module AlphaMiner where

import qualified Data.Set as Set
import Data.Maybe

import Data.Tuple (swap)
import Data.List
import Data.Bifunctor
import Text.XML.Light

type EventLog = [Trace]
type Trace = [Activity]
type Activity = String
type Transition = ([Activity], [Activity])


xmlTest :: String -> IO ()
xmlTest path = do
    s <- readFile path
    case parseXMLDoc s of
        Nothing -> error "Failed to parse xml"
        Just doc -> do

            let traces = findTraces doc
            let eventss = map findEvents traces
            let xs = map (mapMaybe filterEventForActivity) eventss

            let set = Set.fromList xs
            let elog = Set.toList set
            let xls = xLBruteForceLists xs
            print "Reading set:"
            print elog
            putStrLn "\nResulting x_L:"
            print xls
            putStrLn "\nResulting petri net (without start and end transitions):"
            print $ yLLists xls
            putStrLn "\nAll transitions incl. start and end transitions:"
            print $ alphaMiner elog

findTraces :: Element -> [Element]
findTraces = filterChildren (\e -> qName (elName e) == "trace")

findEvents :: Element -> [Element]
findEvents = filterChildren (\e -> qName (elName e) == "event")

filterEventForActivity :: Element -> Maybe String
filterEventForActivity e = line e >>= value
    where
        line = filterChild (\x -> qName (elName x) == "string" && attrVal (head (elAttribs x)) == "concept:name")

value :: Element -> Maybe String
value e = Just $ attrVal (elAttribs e !! 1)

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
alphaMiner elog = (start ++ end) ++ transitions
    where
        start = map (\x -> (["__start__"], [x])) (Set.toList (tI elog))
        end = map (\x -> ([x], ["__end__"])) (Set.toList (tO elog))
        transitions = yLLists $ xLBruteForceLists elog


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