module IOHelper.XESReader
    (
         readXES
    ) where


import Text.XML.Light
    ( parseXMLDoc,
      filterChild,
      filterChildren,
      Attr(attrVal),
      Element(elName, elAttribs),
      QName(qName) )
import Data.Maybe
import Types ( EventLog )

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

            let set = Set.fromList xs
            let elog = Set.toList set
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

readXES :: String -> IO (Maybe EventLog)
readXES path = do
    s <- readFile path
    case parseXMLDoc s of
        Nothing -> return Nothing
        Just doc -> do
            let xs = map (mapMaybe filterEventForActivity . findEvents) (findTraces doc)
            return $ Just xs

findTraces :: Element -> [Element]
findTraces = filterChildren (\e -> qName (elName e) == "trace")

findEvents :: Element -> [Element]
findEvents = filterChildren (\e -> qName (elName e) == "event")

filterEventForActivity :: Element -> Maybe String
filterEventForActivity e = line e >>= value
    where
        line = filterChild (\x -> qName (elName x) == "string" && attrVal (head (elAttribs x)) == "concept:name")

value :: Element -> Maybe String
value e = Just $ attrVal (elAttribs e !! 1)