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
    , description :: String
    , address :: String
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
    return CrawlerRecord
        { uri=link
        , title=(getTitle pagetext)
        , price=(getPrice pagetext)
        , description=""
        , address=""
        , mapuri=""
        , walkscore=0
        , transcore=0
        , wsuri=""
        }

getTitle :: String -> String
getTitle text = head $ yuuko "//title" text

getPrice :: String -> Int
getPrice text = read $ last $ head $ (( head $ yuuko "//body/h2" text ) =~ "^\\$(\\d+) " :: [[String]] ) :: Int
