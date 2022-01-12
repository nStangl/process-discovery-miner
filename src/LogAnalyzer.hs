module LogAnalyzer where

import Types
import Control.Arrow ( Arrow((&&&)) )
import Data.List (sortBy, group, sort)
import Data.Function (on)

countTraces :: EventLog -> [(Int, Trace)]
countTraces = sorted . frequency

frequency :: Ord a => [a] -> [(Int,a)]
frequency =  map (length &&& head) . group . sort

sorted :: Ord a => [(Int, a)] -> [(Int,a)]
sorted = sortBy (flip compare `on` fst)