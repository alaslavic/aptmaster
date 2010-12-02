module CrawlerWorker
( getConfig
)
where

import Database.HDBC.Sqlite3 as DB
import Network
import Network.HTTP
import Network.Browser
import Data.List
import Text.Regex.PCRE as PCRE
import Text.HTML.Yuuko
import Text.HTML.TagSoup
import Text.URI

data CrawlerContext = CrawlerContext
    { config :: CrawlerConfig
    , db :: DB.Connection
    }

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

data CrawlerRecord = CrawlerRecord
    { uri :: String
    , title :: String
    , price :: Int
    , address :: String
    , lat :: Double
    , long :: Double
    , mapuri :: String
    , walkscore :: Int
    , transcore :: Int
    , wsuri :: String
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


fillCLRecord :: String -> IO CrawlerRecord
fillCLRecord link = do
    pagetext <- fetchPage link
    address <- getAddress pagetext
    return CrawlerRecord
        { uri=link
        , title=(getTitle pagetext)
        , price=(getPrice pagetext)
        , address=""
        , lat=0
        , long=0
        , mapuri=""
        , walkscore=0
        , transcore=0
        , wsuri=""
        }

getAddress :: String -> IO String
getAddress raw = return $ head $ collectAddresses  $ extractTexts raw
    where
    extractTexts :: String -> [String]
    extractTexts = (map (\(TagText text) -> text )) . (filter isTagText) . parseTags . head . (yuuko "//body")

getLinkedAddresses :: String -> [String]
getLinkedAddresses = (foldr collapse [] ) (filter wanted) . (map (\x -> parseURI x)) . ( yuuko "//a/@href" )
    where
        collapse :: String -> [String] -> [String]
        collapse href ads =
            let qs = (parseURI href) >>= uriQuery >>= (\x -> return $ queryToPairs x)
            in
                | uri == Nothing -> ads
                | otherwise -> getAd ads uri
        wanted :: Maybe URI -> Bool
        wanted Nothing = False
        wanted Just uri = 

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
