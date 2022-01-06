module IOHelper.XESReader
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

{-
xmlTest :: String -> IO ()
xmlTest path = do
    s <- readFile path
    case parseXMLDoc s of
        Nothing -> error "Failed to parse xml"
        Just doc -> do

            let traces = findTraces doc
            let eventss = map findEvents traces
            let xs = map (mapMaybe filterEventForActivity) eventss

    
            let xls = xLBruteForceLists xs
            print "Reading set:"
            print elog
            putStrLn "\nResulting x_L:"
            print xls
            putStrLn "\nResulting petri net (without start and end transitions):"
            print $ yLLists xls
            putStrLn "\nAll transitions incl. start and end transitions:"
            print $ alphaMiner elog
-}

readXESFile :: String -> IO (Maybe EventLog)
readXESFile path = do
    s <- readFile path
    return $ readXES s

readXES :: XmlSource s => s-> Maybe EventLog
readXES raw =
    case parseXMLDoc raw of
        Nothing -> Nothing
        Just doc -> do
            let traces = findTraces doc

            if isLifecycleExtension doc
                then Just $ map ((mapMaybe getActivityFromEvent . mapMaybe filterEventForLifecycle) . findEvents) traces
                else Just $ map (mapMaybe getActivityFromEvent . findEvents) traces 
            

-- | Checks if the lifecycle extension is being set
isLifecycleExtension :: Element -> Bool
isLifecycleExtension _ = True

findTraces :: Element -> [Element]
findTraces = filterChildren (\e -> qName (elName e) == "trace")

findEvents :: Element -> [Element]
findEvents = filterChildren (\e -> qName (elName e) == "event")

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

