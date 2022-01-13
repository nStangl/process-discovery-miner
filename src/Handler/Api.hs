{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Api where

import Import
import Network.Wai
import Data.Aeson
import Types
import Miner.AlphaMiner
import Miner.RegionMiner ()
import IOHelper.XESReader ( readXES )
import LogAnalyzer ( countTraces )

postAlphaminerV1R :: Handler Value
postAlphaminerV1R =  do
    req <-  waiRequest

    body <- liftIO $ strictRequestBody req
    let elog = readXES body
    case elog of
        Left err -> invalidArgs [pack err]
        Right l -> returnJson $ expToCytoGraph l $ alphaMiner l

postRegionminerV1R :: Handler Value
postRegionminerV1R = do
    returnJson graphNotImplemented


graphNotImplemented :: CytoGraph
graphNotImplemented = CytoGraph 
                        [CytoNode "Not" "ellipse", CytoNode "implemented" "rectangle", CytoNode "yet" "ellipse"] 
                        [CytoEdge "e0" "Not" "implemented" "triangle", CytoEdge "e1" "implemented" "yet" "triangle"]