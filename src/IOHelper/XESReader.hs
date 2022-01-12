module IOHelper.XESReader
    where


import Text.XML.Light
    ( parseXMLDoc,
      filterChild,
      filterChildren,
      elChildren,
      Attr(attrVal),
      Element(elName, elAttribs),
      QName(qName) )
import Data.Maybe ( mapMaybe, fromMaybe )
import Types
import Text.XML.Light.Lexer (XmlSource)
import LogAnalyzer

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
    let elog  = readXES s
    putStrLn "Frequency:"
    print $ countTraces $ fromMaybe [] elog
    return elog

readXES :: XmlSource s => s-> Maybe EventLog
readXES raw =
    case parseXMLDoc raw of
        Nothing -> Nothing
        Just doc -> do
            let traces = findTraces doc
            let (isLife, ats) = isLifecycleExtension doc
            if True
                then Just $ map ((mapMaybe getActivityFromEvent . mapMaybe filterEventForLifecycle) . findEvents) traces
                else Just $ map (mapMaybe getActivityFromEvent . findEvents) traces

readTest :: String -> IO (Bool, [String])
readTest path = do
    s <- readFile path
    case parseXMLDoc s of
        Nothing -> return (False, ["error"])
        Just doc -> return $ isLifecycleExtension doc

-- <string key="lifecycle:transition" value="complete"/>
-- | Checks if the lifecycle extension is being set
isLifecycleExtension :: Element -> (Bool, [String])
isLifecycleExtension doc = do
    (False, [])

-- | Takes the first (any) event and checks if the timestamp attribute is set
isTimestampSet :: Element -> Bool
isTimestampSet event = case line event of
                            Nothing -> False
                            Just _ -> True
    where
        line = filterChild (\x -> qName (elName x) == "date" && attrVal (head (elAttribs x)) == "time:timestamp")

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

getEventLines :: Element -> [(String, String, String)]
getEventLines event = do
    let typef = qName . elName
    let keyf = attrVal . head . elAttribs
    let valuef e = attrVal (elAttribs e !! 1)

    fmap (\x -> (typef x, keyf x, valuef x)) (elChildren event)