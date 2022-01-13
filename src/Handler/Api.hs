{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Api where

import Import
import Network.Wai
import Data.Aeson
import qualified Data.HashMap.Strict as HMS
import Data.Aeson.Types
import Types
import Miner.AlphaMiner
import Miner.RegionMiner ()
import IOHelper.XESReader ( readXES )
import LogAnalyzer ( countTraces )

postAlphaminerV1R :: Handler Value
postAlphaminerV1R =  do
    req <-  waiRequest

    body <- liftIO $ strictRequestBody req
    let logOrErr = readXES body
    case logOrErr of
        Left err -> invalidArgs [pack err]
        Right elog -> do
            let resp = alphaResponse (expToCytoGraph elog (alphaMiner elog)) (countTraces elog)
            return resp

alphaResponse :: CytoGraph -> [(Int, Trace)] -> Value
alphaResponse g tc = Object $ HMS.fromList [("graph", toJSON g), ("traceCount", toJSON tc)]   

postRegionminerV1R :: Handler Value
postRegionminerV1R = do
    returnJson graphNotImplemented

graphNotImplemented :: CytoGraph
graphNotImplemented = CytoGraph 
                        [CytoNode "Not" "ellipse", CytoNode "implemented" "rectangle", CytoNode "yet" "ellipse"] 
                        [CytoEdge "e0" "Not" "implemented" "triangle", CytoEdge "e1" "implemented" "yet" "triangle"]