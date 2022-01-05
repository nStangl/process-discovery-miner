{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Api where

import Import
import Network.Wai
import Network.Wai.Conduit (sourceRequestBody)
import Data.Aeson
import Types
import Miner.AlphaMiner
import Miner.RegionMiner
import IOHelper.XESReader
import IOHelper.JSONHelper
import Network.Wai (responseLBS)



postAlphaminerV1R :: Handler Value
postAlphaminerV1R =  do
    --elog <- requireInsecureJsonBody :: Handler Value
    req <-  waiRequest
    body <- liftIO $ strictRequestBody req

    _ <- print ("postAlphaminerV1R elog value:\n" :: Text)
    _ <- print body
    let elog = readLBSXES body
    case elog of
        Nothing -> returnJson getNode
        Just l -> returnJson $ expToCytoGraph l $ alphaMiner l

postRegionminerV1R :: Handler Value
postRegionminerV1R = do
    returnJson graphNotImplemented


graphNotImplemented :: CytoGraph
graphNotImplemented = CytoGraph 
                        [CytoNode "Not" "ellipse", CytoNode "implemented" "rectangle", CytoNode "yet" "ellipse"] 
                        [CytoEdge "e0" "Not" "implemented" "triangle", CytoEdge "e1" "implemented" "yet" "triangle"]