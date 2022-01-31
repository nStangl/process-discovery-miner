{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- API handlers are defined here.

module Handler.Api where

import Import
import Network.Wai ( strictRequestBody )
import qualified Data.HashMap.Strict as HMS
import Types
import Miner.AlphaMiner
    ( alphaMiner, alphaminersets, createFpMatrix, expToCytoGraph )
import qualified Miner.AlphaPlusMiner as AP
import IOHelper.XESReader ( readXES )
import Data.List ( nub )
import LogStats ( countTraces, logStats ) 

-- | Handler for POST to /api/v1/alphaminer
postAlphaminerV1R :: Handler Value
postAlphaminerV1R =  do
    logOrErr <- parseRequestBodyAndReadXES
    case logOrErr of
        -- return error
        Left err -> sendResponseStatus status400 err
        --invalidArgs [pack err]
        Right elog -> do
            let elog' = nub elog
            let (traceCount, ams, fpm, traceStat, eventStat) = basicAlphaMinerVals elog elog'
            
            let cytoGraph = expToCytoGraph elog' (alphaMiner elog')
            
            return $ Object $ alphaMinerResponse cytoGraph traceCount ams fpm traceStat eventStat

-- | Pack all required values into a HashMap
alphaMinerResponse :: CytoGraph -> [(Int, Trace)] -> AlphaMinerSets -> FootprintMatrix -> Statistics -> Statistics -> HMS.HashMap Text Value
alphaMinerResponse graph traceCount ams fpmatrix traceStat eventStat 
    = HMS.fromList [
        ("graph", toJSON graph),
        ("traceCount", toJSON traceCount),
        ("alphaminersets", toJSON ams),
        ("footprintmatrix", toJSON fpmatrix),
        ("traceStatistics", toJSON traceStat),
        ("eventStatistics", toJSON eventStat)
        ]

-- | Pack all alpha plus miner values into a hashmap
alphaPlusMinerResponse :: CytoGraph -> [(Activity, Activity, Activity)] -> [(Int, Trace)] -> AlphaMinerSets -> FootprintMatrix -> Statistics -> Statistics -> HMS.HashMap Text Value
alphaPlusMinerResponse g l1lps tct ams fpm tS eS 
    = HMS.insert "loopsWithNeighbours" (toJSON l1lps) hm
    where hm = alphaMinerResponse g tct ams fpm tS eS

-- | Handler for POST to /api/v1/alphaplusminer
postAlphaplusminerV1R :: Handler Value
postAlphaplusminerV1R = do
    logOrErr <- parseRequestBodyAndReadXES
    case logOrErr of
        Left err -> sendResponseStatus status400 err
            --invalidArgs [pack err]
        Right elog -> do
            let elog' = nub elog
            let (traceCount, ams, fpm, traceStat, eventStat) = basicAlphaMinerVals elog elog'

            let (ts,l1ls) = AP.alphaPlusMiner elog'
            let cytoGraph = AP.exportToCytoGraph' elog' ts l1ls

            let response = alphaPlusMinerResponse cytoGraph l1ls traceCount ams fpm traceStat eventStat
            return $ Object response

basicAlphaMinerVals :: EventLog -> EventLog -> ([(Int, Trace)], AlphaMinerSets, FootprintMatrix, Statistics, Statistics)
basicAlphaMinerVals elog elog' = let (traceStat, eventStat) = logStats elog elog'
                                 in (countTraces elog, 
                                     alphaminersets elog', 
                                     createFpMatrix elog',
                                     traceStat,
                                     eventStat
                                     )

-- | Parses the body of a POST request and tries to read it's XES content.
-- Returns EventLog if success
parseRequestBodyAndReadXES :: HandlerFor App (Either String EventLog)
parseRequestBodyAndReadXES = do
    req <- waiRequest
    body <- liftIO $ strictRequestBody req
    let logOrErr = readXES body
    return logOrErr

-- | Handler for POST to /api/v1/regionminer
-- return dummy value until implemented
-- Left in because: 
-- 1) Might implement it in future
-- 2) Leaving in does not cause problems; cannot be selected from frontend
postRegionminerV1R :: Handler Value
postRegionminerV1R = do
    returnJson graphNotImplemented

graphNotImplemented :: CytoGraph
graphNotImplemented = CytoGraph 
                        [CytoNode "Not" "ellipse", CytoNode "implemented" "rectangle", CytoNode "yet" "ellipse"] 
                        [CytoEdge "e0" "Not" "implemented" "triangle", CytoEdge "e1" "implemented" "yet" "triangle"]