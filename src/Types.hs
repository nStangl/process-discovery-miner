{-# LANGUAGE OverloadedStrings #-}
module Types
     where

import Data.Aeson
import Control.Applicative ( Alternative(empty) )
import Data.Text ()

type EventLog = [Trace]
type Trace = [Activity]
type Activity = String
type Transition = ([Activity], [Activity])


data CytoNode = CytoNode {
    nodeID :: String,
    shape :: String
} deriving (Show, Eq)

data CytoEdge = CytoEdge {
    edgeID :: String,
    source :: String,
    target :: String,
    arrow :: String
} deriving (Show, Eq)

data CytoGraph = CytoGraph {
    nodes :: [CytoNode],
    edges :: [CytoEdge]
} deriving (Show, Eq)


instance FromJSON CytoNode where
    parseJSON (Object v) = do
        w <- v .: "data"
        n <- w .: "id"
        s <- w .: "shape"
        return CytoNode { nodeID = n, shape = s }
    parseJSON _ = empty

instance ToJSON CytoNode where
    toJSON (CytoNode i s) = object ["data" .= object ["id" .= i, "shape" .= s]]

instance ToJSON CytoEdge where
    toJSON (CytoEdge i s t a) = object ["data" .= object ["id" .= i, "source" .= s, "target" .= t, "arrow" .= a]]

instance FromJSON CytoEdge where
    parseJSON (Object v) = do
        w <- v .: "data"
        i <- w .: "id"
        s <- w .: "source"
        t <- w .: "target"
        a <- w .: "arrow"
        return CytoEdge { edgeID = i, source = s, target = t, arrow = a }
    parseJSON _ = empty

instance ToJSON CytoGraph where
    toJSON (CytoGraph ns es) = object ["nodes" .= ns, "edges" .= es]

instance FromJSON CytoGraph where
    parseJSON (Object v) = do
        ns <- v .: "nodes"
        es <-  v .: "edges"
        return CytoGraph { nodes = ns, edges = es }
    parseJSON _ = empty