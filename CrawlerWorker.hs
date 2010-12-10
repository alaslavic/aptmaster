module CrawlerWorker
( getConfig
)
where

import AptData

import Database.HDBC.Sqlite3 as DB
import Network
import Network.HTTP
import Network.Browser
import Data.List
import Text.Regex.PCRE as PCRE
import Text.URI
import Text.HTML.Yuuko
import Text.HTML.TagSoup
import Text.HJson hiding (toString)
import Text.HJson.Query

data CrawlerConfig = CrawlerConfig
    { gAPI :: String
    , dbtype :: String
    , dbhost :: String
    , dbuser :: String
    , dbpass :: String
    } deriving (Show)

data CrawlerReport = CrawlerReport
    { status :: String
    , new :: Int
    , updated :: Int
    , errors :: [String]
    } deriving (Show)


getConfig :: String -> IO CrawlerConfig

getConfig _ = return CrawlerConfig{ gAPI = "", dbtype = "sqlite", dbhost = "./crawler.db", dbuser = "", dbpass = "" }

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


fillCLRecord :: String -> IO AptRecord
fillCLRecord link = do
    pagetext <- fetchPage link
    (address,lat,lng) <- getAddress pagetext
    (wsuri, ws, ts) <- getWalkScore (address,lat,lng)
    return AptRecord
        { aptUri=link
        , aptTitle=(getTitle pagetext)
        , aptPrice=(getPrice pagetext)
        , aptAddress=(Just address)
        , aptLat=(Just lat)
        , aptLong=(Just lng)
        , aptMapuri=Just ""
        , aptWalkscore=ws
        , aptTranscore=ts
        , aptWsuri=Just wsuri
        }

getWalkScore :: (String,Double,Double) -> IO (String,Maybe Int,Maybe Int)
getWalkScore (address,lat,lng) = 
    let 
        wsuri = "http://www.walkscore.com/score/" ++ (escapeString okInPath (convert address))
        wsgeturi = "http://www.walkscore.com/data/get-walkscore.php?" ++ pairsToQuery [("lat", show lat),("lon",show lng)]
        tsgeturi = "http://www.walkscore.com/data/get-data.php?" ++ pairsToQuery [("req","ts"),("lat", show lat),("lon",show lng),("city", "San Francisco"),("state", "CA"),("cc", "US")]
    in do
    --html <- fetchPage wsuri
    wsjson <- fetchPage wsgeturi
    tsjson <- fetchPage tsgeturi
    return ( wsuri,getws wsjson,getts tsjson )
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
        getPrice text = read $ last $ head $ (( head $ yuuko "//body/h2" text ) =~ "^\\$(\\d+) " :: [[String]] ) :: Int
        convert :: [Char] -> [Char]
        convert = (map swap) . unwords . (map swapWord) . words
        swapWord :: String -> String
        swapWord "&" = "and"
        swapWord x = x
        swap :: Char -> Char
        swap ' ' = '-'
        swap x = x

jParse :: String -> Json
jParse json = case (fromString json) of
    Left l -> jEmpty
    Right j -> j

getAddress :: String -> IO (String,Double,Double)
getAddress raw = 
    let linked = getLinkedAddresses raw
        found = collectAddresses . extractTexts $ raw
    in 
        if linked == [] 
        then do
            firstGeo found
        else do
            (firstGeo linked) `catch` (\e -> firstGeo found )
    where
        firstGeo :: [String] -> IO (String,Double,Double)
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

getGeoCode :: String -> IO (String,Double,Double)
getGeoCode raw = do
    xml <- fetchPage $ "http://maps.googleapis.com/maps/api/geocode/xml?sensor=false&address=" ++ (escapeString okInQuery raw)
    if (status $ head $ yuuko "//status" xml)
        then return $ ((head $ yuuko "//result/formatted_address" xml), ((read $ head $ yuuko "//result/geometry/location/lat" xml) :: Double), ((read $ head $ yuuko "//result/geometry/location/lng" xml) :: Double ))
        else ioError $ userError "Unable to geocode"
    where
        status :: String -> Bool
        status "OK" = True
        status _ = False

getTitle :: String -> String
getTitle text = head $ yuuko "//title" text

getPrice :: String -> Int
getPrice text = read $ last $ head $ (( head $ yuuko "//body/h2" text ) =~ "^\\$(\\d+) " :: [[String]] ) :: Int
