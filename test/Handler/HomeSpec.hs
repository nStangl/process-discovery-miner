{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Homepage" $ do
        it "loads the index" $ do
            get HomeR
            statusIs 200
            -- Would check if the upload field exists, but CSS query is not successfull.
            -- Seems to be an issue with Yesod, cannot handle jsx markup
            -- htmlAnyContain "p" "Upload .xes or .xml file"

    -- test if we can post to API
    describe "API" $ do
        it "POST to alphaminer" $ do
            request $ do
                setMethod "POST"
                setUrl AlphaminerV1R
        
        it "POST to regionmienr" $ do
            request $ do
                setMethod "POST"
                setUrl RegionminerV1R