{-# LANGUAGE OverloadedStrings #-}
module Types
     where

import Data.Aeson
import Control.Applicative ( Alternative(empty) )
import Data.Text ()

type EventLog = [Trace] {- == [[String]] -}
type Trace = [Activity]
type Activity = String
type Transition = ([Activity], [Activity])

data CytoNode = CytoNode {
    nodeID :: String,
    shape :: String
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

data CytoEdge = CytoEdge {
    edgeID :: String,
    source :: String,
    target :: String,
    arrow :: String
} deriving (Show, Eq)

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


data CytoGraph = CytoGraph {
    nodes :: [CytoNode],
    edges :: [CytoEdge]
} deriving (Show, Eq)

instance ToJSON CytoGraph where
    toJSON (CytoGraph ns es) = object ["nodes" .= ns, "edges" .= es]
{-
The FromJSON instance is not used.
Both representation would be possible. If it were used allow for both syntaxes.
instance FromJSON CytoGraph where
    parseJSON (Object v) = do
        ns <- v .: "nodes"
        es <-  v .: "edges"
        return CytoGraph { nodes = ns, edges = es }
    parseJSON _ = empty
-}

instance FromJSON CytoGraph where
    parseJSON (Object v) = do
        g <- v .: "graph"
        ns <- g .: "nodes"
        es <- g .: "edges"
        return CytoGraph { nodes = ns, edges= es }
    parseJSON _ = empty

data AlphaMinerSets = AlphaMinerSets {
    tl :: [Activity],
    ti :: [Activity],
    to :: [Activity],
    xl :: [Transition],
    yl :: [Transition]
}

instance ToJSON AlphaMinerSets where
    toJSON (AlphaMinerSets tl' ti' to' xl' yl')
        = object ["tl" .= tl', "ti" .= ti', "to" .= to', "xl" .= xl', "yl" .= yl']

instance FromJSON AlphaMinerSets where
    parseJSON (Object v) = do
        o <- v .: "alphaminersets"
        tl' <- o .: "tl"
        ti' <- o .: "ti"
        to' <- o .: "to"
        xl' <- o .: "xl"
        yl' <- o .: "yl"
        return $ AlphaMinerSets tl' ti' to' xl' yl'
    parseJSON _ = empty

data FootprintMatrix = FootprintMatrix {
    dim :: Int,
    row :: [String],
    fields :: [[String]]
} deriving (Show, Eq)

instance ToJSON FootprintMatrix where
    toJSON (FootprintMatrix d r xss)
        = object ["dim" .= d, "row" .= r, "fields" .= xss]

instance FromJSON FootprintMatrix where
    parseJSON (Object v) = do
        fpm <- v .: "footprintmatrix"
        d <- fpm .: "dim"
        rs <- fpm .: "row"
        xss <- fpm .: "fields"
        return $ FootprintMatrix d rs xss
    parseJSON _ = empty