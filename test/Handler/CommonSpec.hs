module Handler.CommonSpec (spec) where

import TestImport

-- Check if all static ressources can be found and served
spec :: Spec
spec = withApp $ do
    describe "robots.txt" $ do
        it "gives a 200" $ do
            get RobotsR
            statusIs 200
        it "has correct User-agent" $ do
            get RobotsR
            bodyContains "User-agent: *"
    describe "favicon.ico" $ do
        it "gives a 200" $ do
            get FaviconR
            statusIs 200
    describe "bundle.js" $ do
        it "gives a 200" $ do
            get BundleJSR
            statusIs 200
    describe "mainfest.json" $ do
        it "gives a 200" $ do
            get ManifestR
            statusIs 200
