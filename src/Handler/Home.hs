{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import

-- | Handler for GET /
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        sendFile "text/html" "static/dist/index.html"
