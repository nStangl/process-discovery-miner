module IOHelper.JSONWriter where

import Types
import Miner.AlphaMiner

import GHC.Generics
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON)

ggraph :: CytoGraph
ggraph = CytoGraph {
    nodes = [
        CytoNode { nodeID = "start", shape = "rectangle"},
        CytoNode { nodeID = "n1", shape = "ellipse"},
        CytoNode { nodeID = "end", shape = "rectangle"}
    ],
    edges = [
        CytoEdge {edgeID= "e1", source= "start", target= "n1", arrow= "triangle"},
        CytoEdge {edgeID= "e2", source= "n1", target= "end", arrow= "triangle"}
    ]
}


writeGraphToFile :: IO ()
writeGraphToFile = I.writeFile "myfile.json" (encodeToLazyText ggraph)