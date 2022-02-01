{-# OPTIONS_GHC -Wno-type-defaults #-}
module LogStats (
  logStats,
  countTraces,
  countActivities
) where

import Types
import Data.List (sortBy, group, sort, maximumBy, nub)
import Data.Function ( on )
import Control.Arrow ( Arrow((&&&)) )
import Data.Foldable (minimumBy)


logStats :: EventLog -> EventLog -> (Statistics , Statistics )
logStats elog elog' = (traceStat, eventStat)
    where
        traceStat = traceStatistic elog elog'
        eventStat = eventStatistic elog elog'

traceStatistic :: EventLog -> EventLog -> Statistics
traceStatistic elog elog' =
    let disOc = length elog'
        totOc  = length elog
    in  Statistics {
            totalOccurences    = totOc,
            distinctOccurences = disOc,
            minLength          = minimum (map length elog'),
            maxLength          = maximum (map length elog'),
            avgLengthAll       = truncate' 3 $ fromIntegral (sum (map length elog)) / fromIntegral totOc,
            avgLengthDist      = truncate' 3 $ fromIntegral (sum (map length elog')) / fromIntegral disOc,
            mostFrequent       = combineTrace (snd (maximumBy (compare `on` fst) (frequency elog))),
            leastFrequent      = combineTrace (snd (minimumBy (compare `on` fst) (frequency elog)))
            }

eventStatistic :: EventLog -> EventLog -> Statistics
eventStatistic elog elog' =
    let eventss   = concat elog
        eventss'' = nub $ concat elog'
        lengthEventsAll = length eventss
        lengthEventsDis = length eventss''
    in    Statistics {
            totalOccurences    = lengthEventsAll,
            distinctOccurences = lengthEventsDis,
            minLength          = minimum (map length eventss''),
            maxLength          = maximum (map length eventss''),
            avgLengthAll       = truncate' 3 $ fromIntegral (sum (map length eventss))   / fromIntegral lengthEventsAll,
            avgLengthDist      = truncate' 3 $ fromIntegral (sum (map length eventss'')) / fromIntegral lengthEventsDis,
            mostFrequent       = snd (maximumBy (compare `on` fst) (frequency eventss)),
            leastFrequent      = snd (minimumBy (compare `on` fst) (frequency eventss))
            }
-- | Transform Trace into string
combineTrace :: Trace -> String
combineTrace [] = "<>"
combineTrace (a:as) = "<" ++ a ++ go as
    where
        go []     = ">"
        go (v:vs) = ',' : ' ' : (v ++ go vs)

-- | Cuts off number x after n decimal places
truncate' :: Int -> Float -> Float
truncate' n x = fromIntegral (floor (x * t)) / t
    where 
        t = 10^n

-- | Count Traces for trace count statistics
countTraces :: EventLog -> [(Int, Trace)]
countTraces = sorted . frequency

-- | Count Activities for Pie chart
countActivities :: EventLog -> [(Int, Activity)]
countActivities elog = sorted $ frequency elog'
    where elog' = concat elog

-- | Counts the Frequency of elements in a list
frequency :: Ord a => [a] -> [(Int,a)]
frequency =  map (length &&& head) . group . sort

sorted :: Ord a => [(Int, a)] -> [(Int,a)]
sorted = sortBy (flip compare `on` fst)