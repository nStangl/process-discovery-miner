module IOHelper.XESReader
    (
         readXESFile,
         readXES,
         readBSXES,
         readLBSXES
    ) where


import Text.XML.Light
    ( parseXMLDoc,
      filterChild,
      filterChildren,
      Attr(attrVal),
      Element(elName, elAttribs),
      QName(qName) )
import Data.Maybe
import Types
import Import (ByteString)
import qualified Data.ByteString.Lazy as LBS
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

readXESFile :: String -> IO (Maybe EventLog)
readXESFile path = do
    s <- readFile path
    return $ readXES s


readXES :: String -> Maybe EventLog
readXES raw = do
    case parseXMLDoc raw of
        Nothing -> Nothing
        Just doc -> do
            let xs = map (mapMaybe filterEventForActivity . findEvents) (findTraces doc)
            Just xs

readBSXES :: ByteString -> Maybe EventLog
readBSXES rawbs = do
    case parseXMLDoc rawbs of
        Nothing -> Nothing
        Just doc -> do
            let xs = map (mapMaybe filterEventForActivity . findEvents) (findTraces doc)
            Just xs

readLBSXES :: LBS.ByteString -> Maybe EventLog
readLBSXES raw = do
    case parseXMLDoc raw of
        Nothing -> Nothing
        Just doc -> do
            let xs = map (mapMaybe filterEventForActivity . findEvents) (findTraces doc)
            Just xs


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