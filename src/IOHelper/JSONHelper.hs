{-# LANGUAGE OverloadedStrings #-}

module IOHelper.JSONHelper where

import Data.Aeson
import Data.Text ()

import Miner.AlphaMiner
import Types 

getGraph :: CytoGraph
getGraph = graph

getNode :: CytoNode
getNode  = CytoNode { nodeID = "start", shape = "rectangle"}

decodeNode :: Maybe CytoNode
decodeNode = decode "{\"data\":{\"shape\":\"rectangle\",\"id\":\"start\"}}" :: Maybe CytoNode

