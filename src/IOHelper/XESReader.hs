module IOHelper.XESReader
    ( readXESFile,
      readXES
    )
    where

import Text.XML.Light
    ( parseXMLDoc,
      filterChild,
      filterChildren,
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
            return logOrErr

-- | Read the XES files contens from a XmlSource
readXES :: XmlSource s => s-> Either String EventLog
readXES raw =
    case parseXMLDoc raw of
        Nothing -> Left readXESError
        Just doc -> do
            let traces = findTraces doc
            Right $ map ((mapMaybe getActivityFromEvent . mapMaybe filterEventForLifecycle) . findEvents) traces
            --Right $ map (mapMaybe getActivityFromEvent . findEvents) traces

-- | Takes the first (any) event and checks if the timestamp attribute is set
isTimestampSet :: Element -> Bool
isTimestampSet event = case line event of
                            Nothing -> False
                            Just _ -> True
    where
        line = filterChild (\x -> qName (elName x) == "date" && attrVal (head (elAttribs x)) == "time:timestamp")

-- | Given the <log..> element, find all its child <trace..> elements
findTraces :: Element -> [Element]
findTraces = filterChildren (\e -> qName (elName e) == "trace")

-- | Given a <trace..> element, find all its child <event..> elements
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

