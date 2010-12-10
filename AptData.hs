module AptData
( AptRecord(AptRecord)
, aptUri
, aptTitle
, aptPrice
, aptAddress
, aptLat
, aptLong
, aptMapuri
, aptWalkscore
, aptTranscore
, aptWsuri
)
where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List

data AptRecord = AptRecord
    { aptUri :: String
    , aptTitle :: String
    , aptPrice :: Int
    , aptAddress :: Maybe String
    , aptLat :: Maybe Double
    , aptLong :: Maybe Double
    , aptMapuri :: Maybe String
    , aptWalkscore :: Maybe Int
    , aptTranscore :: Maybe Int
    , aptWsuri :: Maybe String
    } deriving (Show)

data AptConnection = AptConnection
{ aptDB = Connection
}

type AptDBUser = String
type AptDBPw = String
type AptDBType = String
type AptDBHost = String

aptConnect :: AptDBHost -> AptDBUser -> AptDBPw -> AptDBType -> AptConnection
aptConnect host user pass typ =
    case (typ) of
        "sqlite" -> do
            conn <- connectSqlite3 host
        _ -> ioError $ userError $ "unsupported db type '" ++ typ ++ "'"
