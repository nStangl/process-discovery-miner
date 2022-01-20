module IOHelper.XESReader
    ( readXESFile,
      readXES,
      readTest
    )
    where

import Text.XML.Light
    ( parseXMLDoc,
      filterChild,
      filterChildren,
      elChildren,
      Attr(attrVal),
      Element(elName, elAttribs),
      QName(qName) )
import Data.Maybe ( mapMaybe )
import Types
import Text.XML.Light.Lexer (XmlSource)
import Control.Exception ( try, SomeException )

readXESFileError :: String
readXESFileError = "An error occured trying to read the file! This might be caused by a wrong encoding."

readXESError :: String
readXESError = "An error occured trying to parse the xml! Make sure it is a valid xml document and is defined as seen in the example."

-- | Read the XES file at given file.
-- Path can lead to in-memory file or a temporary file depending on file size.
readXESFile :: String -> IO (Either String EventLog)
readXESFile path = do
    strOrExce <- try $ readFile path :: IO (Either SomeException String)
    case strOrExce of
        Left _  -> return $ Left readXESFileError
        Right s -> do
            let logOrErr = readXES s
            return $ case logOrErr of
                Left err -> Left err
                Right elog -> Right elog

-- | Read the XES files contens from a XmlSource
readXES :: XmlSource s => s-> Either String EventLog
readXES raw =
    case parseXMLDoc raw of
        Nothing -> Left readXESError
        Just doc -> do
            let traces = findTraces doc
            -- TODO: Replace?
            if True
                then Right $ map ((mapMaybe getActivityFromEvent . mapMaybe filterEventForLifecycle) . findEvents) traces
                else Right $ map (mapMaybe getActivityFromEvent . findEvents) traces

-- | TODO: remove
readTest :: String -> IO (Bool, [String])
readTest path = do
    s <- readFile path
    case parseXMLDoc s of
        Nothing -> return (False, ["error"])
        Just doc -> return $ isLifecycleExtension doc

-- | Checks if the lifecycle extension is being set
isLifecycleExtension :: Element -> (Bool, [String])
isLifecycleExtension _ = do
    (False, [])

-- | Takes the first (any) event and checks if the timestamp attribute is set
isTimestampSet :: Element -> Bool
isTimestampSet event = case line event of
                            Nothing -> False
                            Just _ -> True
    where
        line = filterChild (\x -> qName (elName x) == "date" && attrVal (head (elAttribs x)) == "time:timestamp")

-- | Given the <log..> element, find all it's child <trace..> elements
findTraces :: Element -> [Element]
findTraces = filterChildren (\e -> qName (elName e) == "trace")

-- | Given a <trace..> element, find all it's child <event..> elements
findEvents :: Element -> [Element]
findEvents = filterChildren (\e -> qName (elName e) == "event")

-- | Extract the activity name from a given Element
getActivityFromEvent :: Element -> Maybe String
getActivityFromEvent e = line e >>= value
    where
        value :: Element -> Maybe String
        value ele = Just $ attrVal (elAttribs ele !! 1)
        line = filterChild (\x -> qName (elName x) == "string" && attrVal (head (elAttribs x)) == "concept:name")

-- | Returns events where if 'lifecycle:transition' exists it is set to 'complete'
filterEventForLifecycle :: Element -> Maybe Element
filterEventForLifecycle eve = case line eve of
                                -- 'lifecycle:transition' does not exist, return event
                                Nothing -> Just eve
                                -- 'lifecycle:transition' does exist, return when value is 'complete'
                                Just lifeLine-> if attrVal (elAttribs lifeLine !! 1) == "complete" then Just eve else Nothing
    where
        line = filterChild (\e -> qName (elName e) == "string" && attrVal (head (elAttribs e)) == "lifecycle:transition")

-- | TODO: Leave in or replace?
getEventLines :: Element -> [(String, String, String)]
getEventLines event = do
    let typef = qName . elName
    let keyf = attrVal . head . elAttribs
    let valuef e = attrVal (elAttribs e !! 1)

    fmap (\x -> (typef x, keyf x, valuef x)) (elChildren event)