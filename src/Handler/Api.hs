{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- API handlers are defined here.

module Handler.Api where

import Import
import Network.Wai
import qualified Data.HashMap.Strict as HMS
import Types
import Miner.AlphaMiner
import Miner.RegionMiner ()
import IOHelper.XESReader ( readXES )
import LogAnalyzer ( countTraces )

postAlphaminerV1R :: Handler Value
postAlphaminerV1R =  do
    -- read the request body
    req <-  waiRequest
    body <- liftIO $ strictRequestBody req
    let logOrErr = readXES body
    -- check for error and return error message if needed
    case logOrErr of
        Left err -> invalidArgs [pack err]
        Right elog -> do
            let cytoGraph = expToCytoGraph elog (alphaMiner elog)
            let traceCount = countTraces elog
            let ams = alphaminersets elog
            let fpm = createFpMatrix elog
            return $ alphaminerResp cytoGraph traceCount ams fpm

-- Pack all values from the alphaminer into a (JSON) Value
alphaminerResp :: CytoGraph -> [(Int, Trace)] -> AlphaMinerSets -> FootprintMatrix -> Value
alphaminerResp g tc ams fpmatrix = Object $ HMS.fromList [
            ("graph", toJSON g),
            ("traceCount", toJSON tc),
            ("alphaminersets", toJSON ams),
            ("footprintmatrix", toJSON fpmatrix)
            ]   

-- return dummy value until implemented
postRegionminerV1R :: Handler Value
postRegionminerV1R = do
    returnJson graphNotImplemented

graphNotImplemented :: CytoGraph
graphNotImplemented = CytoGraph 
                        [CytoNode "Not" "ellipse", CytoNode "implemented" "rectangle", CytoNode "yet" "ellipse"] 
                        [CytoEdge "e0" "Not" "implemented" "triangle", CytoEdge "e1" "implemented" "yet" "triangle"]