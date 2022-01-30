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
import IOHelper.XESReader ( readXES, countTraces )
import Data.List ( nub )

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
            let (traceCount, ams, fpm) = basicAlphaMinerVals elog elog'
            
            let cytoGraph = expToCytoGraph elog' (alphaMiner elog')
            return $ alphaminerResp cytoGraph traceCount ams fpm

-- | Pack all values from the alphaminer into a (JSON) Value
alphaminerResp :: CytoGraph -> [(Int, Trace)] -> AlphaMinerSets -> FootprintMatrix -> Value
alphaminerResp g tc ams fpmatrix = Object $ HMS.fromList [
            ("graph", toJSON g),
            ("traceCount", toJSON tc),
            ("alphaminersets", toJSON ams),
            ("footprintmatrix", toJSON fpmatrix)
            ]   

-- | Handler for POST to /api/v1/alphaplusminer
postAlphaplusminerV1R :: Handler Value
postAlphaplusminerV1R = do
    logOrErr <- parseRequestBodyAndReadXES
    case logOrErr of
        Left err -> sendResponseStatus status400 err
            --invalidArgs [pack err]
        Right elog -> do
            let elog' = nub elog
            let (traceCount, ams, fpm) = basicAlphaMinerVals elog elog'

            let (ts,l1ls) = AP.alphaPlusMiner elog'
            let cytoGraph = AP.exportToCytoGraph' elog' ts l1ls

            return $ alphaplusminerResp cytoGraph l1ls traceCount ams fpm

basicAlphaMinerVals :: EventLog -> EventLog -> ([(Int, Trace)], AlphaMinerSets, FootprintMatrix)
basicAlphaMinerVals elog elog' = (countTraces elog, alphaminersets elog', createFpMatrix elog')

-- | Pack all values from the alphplusaminer into a (JSON) Value
alphaplusminerResp :: CytoGraph -> [(Activity,Activity,Activity)] -> [(Int, Trace)] -> AlphaMinerSets -> FootprintMatrix -> Value
alphaplusminerResp g l1ls tc ams fpmatrix = Object $ HMS.fromList [
            ("graph", toJSON g),
            ("loopsWithNeighbours", toJSON l1ls),
            ("traceCount", toJSON tc),
            ("alphaminersets", toJSON ams),
            ("footprintmatrix", toJSON fpmatrix)
            ]   

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