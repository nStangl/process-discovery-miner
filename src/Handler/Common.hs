{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

-- Embed files in the executable at compile time to avoid runtime
-- dependecy, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 12 -- cache for 12 hours
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

getBundleJSR :: Handler TypedContent
getBundleJSR = return $ TypedContent typeJavascript
                      $ toContent $(embedFile "static/dist/bundle.js")

getManifestR :: Handler TypedContent
getManifestR = return $ TypedContent typeJson
                      $ toContent $(embedFile "static/app/public/manifest.json")