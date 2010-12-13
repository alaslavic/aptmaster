module AptData
( AptRecord(AptRecord)
, aptUri
, aptTitle
, aptPrice
, aptAddress
, aptNeighborhood
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
    , aptNeighborhood :: Maybe String
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

aptQuery

aptConnect :: AptDBHost -> AptDBUser -> AptDBPw -> AptDBType -> IO Connection
aptConnect host user pass typ =
    case (typ) of
        "sqlite" -> do
            conn <- connectSqlite3 host
            valid <- (aptValidateDB conn) `catch` (makeDB conn)
            case (valid) of 
                True -> conn
                False -> ioError $ userError $ "unable to validate connection"
        _ -> ioError $ userError $ "unsupported db type '" ++ typ ++ "'"

aptValidateDB :: Connection -> IO Bool
aptValidateDB conn = do 
    getvsn <- (prepare conn "select version from schema_version")
    execute getvsn []
    rslt <- fetchAllRows' getvsn
    case (head . head $ rslt) of
        _ -> True

makeDB :: Connection -> IO Bool
makeDB connection = do
    run "CREATE TABLE IF NOT EXISTS schema_version ( version integer )" []
    run "insert into schema_version (version) values (1)" []
    run "CREATE TABLE IF NOT EXISTS poi ( id integer, name varchar(255), lat double, long double )" []
    run "CREATE TABLE IF NOT EXISTS apt ( id integer, uri text, title text, price integer, address text, neighborhood, lat double, long double, mapuri text, ws integer, ts integer, wsuri text )" []
    commit conn
    return True
