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
    ( alphaMiner, alphaminersets, createFpMatrix, expToCytoGraph )
import qualified Miner.AlphaPlusMiner as AP
import Miner.RegionMiner ()
import IOHelper.XESReader ( readXES, countTraces )
import Data.List ( nub )

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
            let elog' = nub elog
            let cytoGraph = expToCytoGraph elog' (alphaMiner elog')
            let traceCount = countTraces elog
            let ams = alphaminersets elog'
            let fpm = createFpMatrix elog'
            return $ alphaminerResp cytoGraph traceCount ams fpm

-- | Pack all values from the alphaminer into a (JSON) Value
alphaminerResp :: CytoGraph -> [(Int, Trace)] -> AlphaMinerSets -> FootprintMatrix -> Value
alphaminerResp g tc ams fpmatrix = Object $ HMS.fromList [
            ("graph", toJSON g),
            ("traceCount", toJSON tc),
            ("alphaminersets", toJSON ams),
            ("footprintmatrix", toJSON fpmatrix)
            ]   

postAlphaplusminerV1R :: Handler Value
postAlphaplusminerV1R = do
    -- read request body
    req <- waiRequest
    body <- liftIO $ strictRequestBody req
    let logOrErr = readXES body
    case logOrErr of
        Left err -> invalidArgs [pack "no"]
        Right elog -> do
            let elog' = nub elog
            let (ts,l1ls) = AP.alphaPlusMiner elog'
            let cytoGraph = AP.exportToCytoGraph' elog' ts l1ls
            let traceCount = countTraces elog
            let ams = alphaminersets elog'
            let fpm = createFpMatrix elog'

            return $ alphaminerResp cytoGraph traceCount ams fpm



-- return dummy value until implemented
postRegionminerV1R :: Handler Value
postRegionminerV1R = do
    returnJson graphNotImplemented

graphNotImplemented :: CytoGraph
graphNotImplemented = CytoGraph 
                        [CytoNode "Not" "ellipse", CytoNode "implemented" "rectangle", CytoNode "yet" "ellipse"] 
                        [CytoEdge "e0" "Not" "implemented" "triangle", CytoEdge "e1" "implemented" "yet" "triangle"]