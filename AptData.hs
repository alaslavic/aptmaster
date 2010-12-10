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
, AptConnection(AptConnection)
, aptDB
, aptConnect
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
    { aptDB :: Connection
    }

type AptDBUser = String
type AptDBPw = String
type AptDBType = String
type AptDBHost = String

aptConnect :: AptDBHost -> AptDBUser -> AptDBPw -> AptDBType -> IO AptConnection
aptConnect host user pass typ =
    case (typ) of
        "sqlite" -> do
            conn <- connectSqlite3 host
            valid <- aptValidateDB conn
            case (valid) of 
                True -> makeAptConnection conn
                False -> ioError $ userError $ "unable to validate connection"
        _ -> ioError $ userError $ "unsupported db type '" ++ typ ++ "'"

makeAptConnection :: Connection -> IO AptConnection
makeAptConnection c = do
    return AptConnection {
          aptDB=c
        }

aptValidateDB :: Connection -> IO Bool
aptValidateDB conn = do 
    getvsn <- (prepare conn "select version from schema_version") `catch` (\e -> return . aptValidateDB . makeDB conn)
    execute getvsn
    rslt <- fetchAllRows' getvsn
    case (head . head $ rslt) of
