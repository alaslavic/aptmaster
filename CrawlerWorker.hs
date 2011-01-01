module CrawlerWorker
( getCLLinks
, addPoi
, fetchPage
, fillCLRecord
, linkId
, poiCalc
, crawl
, CrawlerConfig
, debug
)
where

import AptData

--import Database.HDBC.Sqlite3
import Database.HDBC.MySQL
import Database.HDBC
import Network
import Network.HTTP
import Network.Browser
import Data.List
import Text.Regex.PCRE as PCRE
import Text.URI
import Text.HTML.Yuuko
import Text.HTML.TagSoup
import Text.HJson hiding (toString)
import Text.HJson.Query hiding (debug)
import System.Console.GetOpt
import qualified Data.Map as Map
import System (getArgs)
import Data.Maybe


type CrawlerConfig = Map.Map String String

debug :: CrawlerConfig -> String -> IO ()
debug config message = do
    case (Map.member "verbose" config) of
        True -> putStrLn message
        False -> return ()

crawl :: CrawlerConfig ->  IO [(String, IO Bool)]
crawl config = do
    let d = debug config
    d $ show config
    let host = Map.findWithDefault "localhost" "dbhost" config
    let inst = Map.findWithDefault "test" "dbinst" config
    let user = Map.findWithDefault "" "dbuser" config
    let pass = Map.findWithDefault "" "dbpass" config
    let typ = Map.findWithDefault "mysql" "dbtype" config
    conn <- aptConnect host inst user pass typ
    poi <- aptPoiQuery conn [] []
    d $ show poi
    links <- getCLLinks $ Map.findWithDefault "http://sfbay.craigslist.org/sfc/apa/" "clpage" config
    d $ show links
    exist <- sequence $ map (\l -> aptExists conn $ linkId l) links
    let bools = map not exist
    d $ show bools 
    let wanted = map (\x -> fst x ) $ filter (\x -> snd x ) $ zip links bools
    let filled = map (\link -> fillCLRecord link) wanted
    let poied = map (\apt -> (return . (poiCalc poi)) =<< apt ) filled
    let inserted = map (\apt -> (aptPutCommit conn) =<< apt ) poied
    return $ zip wanted inserted

addPoi :: CrawlerConfig -> String -> String -> IO AptPoi
addPoi config t addstr = do
    let d = debug config
    d $ show config
    let host = Map.findWithDefault "localhost" "dbhost" config
    let user = Map.findWithDefault "" "dbuser" config
    let pass = Map.findWithDefault "" "dbpass" config
    let typ = Map.findWithDefault "mysql" "dbtype" config
    let inst = Map.findWithDefault "test" "dbinst" config
    conn <- aptConnect host inst user pass typ
    d $ "attempting to geocode " ++ addstr
    (address, lat, lng) <- getGeoCode addstr
    geocode <- getGeoCode addstr
    d $ "geocoded to " ++ (show geocode)
    case geocode of
        (Nothing,Nothing,Nothing) -> ioError $ userError "unable to geocode"
        (Just address, Just lat, Just lng) -> do
            let rec = AptPoi{ aptPoiType=t, aptPoiAddress=address, aptPoiLat=lat, aptPoiLong=lng, aptPoiId=0 }
            d $ "attempting insert " ++ (show rec)
            put <- aptPoiPut conn rec
            commit conn
            d $ "record inserted " ++ (show rec)
            rec' <- aptPoiQuery conn [Field "address",Equ, Value $ toSql address, And, Field "type", Equ, Value $ toSql t] []
            d $ "record verified " ++ (show rec')
            case rec' of
                [r] -> do return r
                _ -> do ioError $ userError $ "unable to insert poi of " ++ typ ++ " " ++ address


fetchPage :: String -> IO String
fetchPage url = do
    rsp <- simpleHTTP $ getRequest url
    getResponseBody rsp

getCLLinks :: String -> IO [String]
getCLLinks url = do
    pagetext <- fetchPage url
    return $ filter matchlink $ yuuko "//@href" pagetext 
    where 
        matchlink = match ( makeRegexOpts defaultCompOpt defaultExecOpt "/\\d+\\.html$" :: Regex )

linkId :: String -> Int
linkId link = read $ last $ head ( link =~ "(\\d+)\\.html$" :: [[String]] ) ::Int


fillCLRecord :: String -> IO AptRecord
fillCLRecord link = do
    let id = linkId link 
    pagetext <- fetchPage link
    let (title,hood,price) = case (getParts pagetext) of
            Nothing -> ("","",0)
            (Just (x,y,z)) -> (x,y,z)
    (address,lat,lng) <- getAddress pagetext
    (wsuri, ws, ts) <- getWalkScore (address,lat,lng)
    case title of
        "" -> ioError $ userError $ "unable to fill record for: " ++ link
        _ -> 
            return AptRecord
                { aptId=id
                , aptUri=link
                , aptTitle=title
                , aptPrice=price
                , aptAddress=address
                , aptNeighborhood=Just hood
                , aptLat=lat
                , aptLong=lng
                , aptMapuri=Just ""
                , aptWalkscore=ws
                , aptTranscore=ts
                , aptWsuri=wsuri
                , aptPois=[]
                }
 
getWalkScore :: (Maybe String,Maybe Double,Maybe Double) -> IO (Maybe String,Maybe Int,Maybe Int)
getWalkScore (Just address,Just lat,Just lng) = 
    let 
        wsuri = "http://www.walkscore.com/score/" ++ (escapeString okInPath (convert address))
        wsgeturi = "http://www.walkscore.com/data/get-walkscore.php?" ++ pairsToQuery [("lat", show lat),("lon",show lng)]
        tsgeturi = "http://www.walkscore.com/data/get-data.php?" ++ pairsToQuery [("req","ts"),("lat", show lat),("lon",show lng),("city", "San Francisco"),("state", "CA"),("cc", "US")]
    in do
    wsjson <- fetchPage wsgeturi
    tsjson <- fetchPage tsgeturi
    return ( Just wsuri,getws wsjson,getts tsjson )
    where
        getws :: String -> Maybe Int
        getws json = 
            let
                jObj = jParse json
            in
                case ( (getFromKey "walkscore") >>> isNum $ jObj) of
                    ((JNumber x):_) -> Just (ceiling x)
                    _ -> Nothing
        getts :: String -> Maybe Int
        getts json = 
            let 
                jObj = jParse json
            in
                case ( (getFromKey "ts") >>> (getFromKey "transit_score") >>> isNum $ jObj) of
                    ((JNumber x):_) -> Just (ceiling x)
                    _ -> Nothing
        convert :: [Char] -> [Char]
        convert = (map swap) . unwords . (map swapWord) . words
        swapWord :: String -> String
        swapWord "&" = "and"
        swapWord x = x
        swap :: Char -> Char
        swap ' ' = '-'
        swap x = x
getWalkScore x = return (Nothing,Nothing,Nothing)

jParse :: String -> Json
jParse json = case (fromString json) of
    Left l -> JNull
    Right j -> j

poiCalc :: [AptPoi] -> AptRecord -> AptRecord
poiCalc ps r = case ( aptLat r, aptLong r ) of 
    (Nothing,Nothing) -> r
    (Just lat, Just lng) -> r { aptPois=( map (\p -> (p, haversine lat lng (aptPoiLat p) (aptPoiLong p)  ) ) ) ps }

haversine :: Double -> Double -> Double -> Double -> Double
haversine lat1 lng1 lat2 lng2 = 
    let
        lat1' = pi * (lat1 / 180)
        lng1' = pi * (lng1 / 180)
        lat2' = pi * (lat2 / 180)
        lng2' = pi * (lng2 / 180)
        dlat = lat2' - lat1'
        dlng = lng2' - lng1'
        a = ((sin (dlat/2))^2) + ((cos lat1') * (cos lat2') * ((sin (dlng/2) )^2) )
        c = 2 * (atan2 ( sqrt a ) ( sqrt 1/a ) )
    in
        r * c
    where 
        r = 3956.6

getAddress :: String -> IO (Maybe String,Maybe Double,Maybe Double)
getAddress raw = 
    let linked = getLinkedAddresses raw
        found = collectAddresses . extractTexts $ raw
    in 
        if linked == [] 
        then do
            firstGeo found `catch` (\e -> return (Nothing, Nothing, Nothing))
        else do
            ((firstGeo linked) `catch` (\e -> firstGeo found )) `catch` (\e -> return (Nothing, Nothing, Nothing) )
    where
        firstGeo :: [String] -> IO (Maybe String,Maybe Double,Maybe Double)
        firstGeo [] = ioError $ userError "unable to geocode any"
        firstGeo (x:xs) = do
            (getGeoCode x) `catch` (\e -> firstGeo xs)
        extractTexts :: String -> [String]
        extractTexts = (map (\(TagText text) -> text )) . (filter isTagText) . parseTags . head . (yuuko "//body")
            
getLinkedAddresses :: String -> [String]
getLinkedAddresses = (foldr collapse [] ) . (filter wanted) . (map (\x -> parseURI x)) . ( yuuko "//a/@href" )
    where
        collapse :: Maybe URI -> [String] -> [String]
        collapse Nothing ads = ads
        collapse href ads =
            let qs = href >>= uriQuery >>= (\x -> Just (queryToPairs x))
            in getAd ads qs
        wanted :: Maybe URI -> Bool
        wanted Nothing = False
        wanted (Just uri) = wantedHosts . uriRegName $ uri
        wantedHosts :: Maybe String -> Bool
        wantedHosts Nothing = False
        wantedHosts (Just host) = ( host =~ "(?i)(maps.google.com)|(maps.yahoo.com)" :: Bool )
        getAd :: [String] -> Maybe [(String,String)] -> [String]
        getAd ads Nothing = ads
        getAd ads (Just xs) = (head $ map (\x -> snd x) $ filter adFilter xs):ads
        adFilter :: (String,String) -> Bool
        adFilter ("q",_) = True
        adFilter ("addr",_) = True
        adFilter _ = False

collectAddresses :: [String] -> [String]
collectAddresses = foldr collect [] 
    where
    collect :: String -> [String] -> [String]
    collect consider ps = ps

getGeoCode :: String -> IO (Maybe String,Maybe Double,Maybe Double)
getGeoCode raw = do
    xml <- fetchPage $ "http://maps.googleapis.com/maps/api/geocode/xml?sensor=false&address=" ++ (escapeString okInQuery raw)
    case (head $ yuuko "//status" xml) of
        "OK" -> return $ (Just (head $ yuuko "//result/formatted_address" xml), (Just (read $ head $ yuuko "//result/geometry/location/lat" xml) :: Maybe Double), (Just (read $ head $ yuuko "//result/geometry/location/lng" xml) :: Maybe Double ))
        _ -> ioError $ userError "Unable to geocode"
    where
        status :: String -> Bool
        status "OK" = True
        status _ = False

--getTitle :: String -> String
--getTitle text = head $ yuuko "//title" text

--getPrice :: String -> Int
--getPrice text = read $ last $ head $ (( head $ yuuko "//body/h2" text ) =~ "^\\$(\\d+) " :: [[String]] ) :: Int

getParts :: String -> Maybe (String, String, Int)
getParts t = 
    let
        title = head $ yuuko "//title" t
        h2 = head $ yuuko "//body/h2" t
        m = (h2 =~ "^(?:\\$(\\d+))?[^\\(]+(?:\\((.+?)\\))?" :: [[String]])
    in
        case title of
            "" -> Nothing
            _ -> case m of
                [[_,"",_]] -> Nothing
                [[_,p,n]] -> Just (title, n, (read p :: Int))
                _ -> Nothing

