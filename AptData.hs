module AptData
( AptRecord(AptRecord)
, aptId
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
, AptPoi(AptPoi)
, aptPoiId
, aptPoiType
, aptPoiAddress
, aptPoiLat
, aptPoiLong
, AptConnection(AptConnection)
, aptDB
, aptConnect
, aptRecToSql
, sqlToAptRec
, aptPut
, aptPutCommit
, aptQuery
, aptPoiPut
, aptPoiPutCommit
, aptPoiTypeQuery
)
where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List

data AptRecord = AptRecord
    { aptId :: Int
    , aptUri :: String
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

data AptPoi = AptPoi
    { aptPoiId :: Int
    , aptPoiType :: String
    , aptPoiAddress :: Maybe String
    , aptPoiLat :: Maybe Double
    , aptPoiLong :: Maybe Double
    } deriving (Show)

data AptConnection = AptConnection
    { aptDB :: Connection
    }

type AptDBUser = String
type AptDBPw = String
type AptDBType = String
type AptDBHost = String

aptRecToSql :: AptRecord -> [SqlValue]
aptRecToSql rec = 
    [toSql $ aptId rec
    ,toSql $ aptUri rec
    ,toSql $ aptTitle rec
    ,toSql $ aptPrice rec
    ,toSql $ aptAddress rec
    ,toSql $ aptNeighborhood rec
    ,toSql $ aptLat rec
    ,toSql $ aptLong rec
    ,toSql $ aptMapuri rec
    ,toSql $ aptWalkscore rec
    ,toSql $ aptTranscore rec
    ,toSql $ aptWsuri rec
    ]

aptPoiToSql :: AptPoi -> [SqlValue]
aptPoiToSql rec = 
    [toSql $ aptPoiId rec
    ,toSql $ aptPoiType rec
    ,toSql $ aptPoiAddress rec
    ,toSql $ aptPoiLat rec
    ,toSql $ aptPoiLong rec
    ]

sqlToAptRec :: [SqlValue] -> AptRecord
sqlToAptRec [id,uri,tit,pri,add,nei,lat,lng,mu,ws,ts,wsu] = AptRecord 
    { aptId=(fromSql id)
    , aptUri=(fromSql uri)
    , aptTitle=(fromSql tit)
    , aptPrice=(fromSql pri)
    , aptAddress=(fromSql add)
    , aptNeighborhood=(fromSql nei)
    , aptLat=(fromSql lat)
    , aptLong=(fromSql lng)
    , aptMapuri=(fromSql mu)
    , aptWalkscore=(fromSql ws)
    , aptTranscore=(fromSql ts)
    , aptWsuri=(fromSql wsu)
    }

sqlToAptPoi :: [SqlValue] -> AptPoi
sqlToAptPoi [id,typ,add,lat,lng] = AptPoi
    { aptPoiId=(fromSql id)
    , aptPoiType=(fromSql typ)
    , aptPoiAddress=(fromSql add)
    , aptPoiLat=(fromSql lat)
    , aptPoiLong=(fromSql lng)
    }

aptPoiTypeQuery :: Connection -> IO [String]
aptPoiTypeQuery conn = do
    stmt <- prepare conn "select distinct type from poi order by type asc"
    rslt <- execute stmt []
    rows <- fetchAllRows' stmt
    return $ map (\r -> fromSql (head r) :: String ) rows

aptPoiPut :: Connection -> AptPoi -> IO Bool
aptPoiPut conn rec = do
    stmt <- prepare conn "insert into poi( type, address, lat, lng ) values (?,?,?,?)"
    rslt <- execute stmt $ aptPoiToSql rec
    case rslt of 
        1 -> return True
        _ -> return False

aptPoiPutCommit :: Connection -> AptPoi -> IO Bool
aptPoiPutCommit conn rec = do
    r <- aptPoiPut conn rec
    commit conn
    return r

-- aptQueryComplex :: Connection -> [String] -> [String] Int -> IO [AptRecord]
-- aptQueryComplex conn types o page =
--     let
--         op = "select"
--         (fields,from,order) = checkOrder o 
--     in do
--         stmt <- prepare conn makeStmt op fields from types order
--     where
--         checkOrder :: [String] -> ([String],[String],String)
--         checkOrder os = foldr doOrder (defFields,["apt"],[]) os
--         doOrder :: String -> ([String],[String],[String]) -> ([String],[String],[String])
--         doOrder s (fields,from,order) = case s of
--             _ -> (fields,from,order)
--         defFields :: [String]
--         defFields = ["apt.id", "apt.uri", "apt.title", "apt.price", "apt.address", "apt.neighborhood", "apt.lat", "apt.lng", "apt.mapuri", "apt.ws", "apt.ts", "apt.wsuri" ]
--         makeStmt :: String -> [String] -> [String] -> [String] -> [String] -> Int -> String
--         makeStmt op fields from types order page = concat [op, " from ", 

aptQuery :: Connection -> Int -> IO [AptRecord]
aptQuery conn page = do
    stmt <- prepare conn "select \
\apt.id, apt.uri, apt.title,apt.price,apt.address,apt.neighborhood,apt.lat,apt.lng,apt.mapuri,apt.ws,apt.ts,apt.wsuri \
\from apt \
\order by apt.price \
\limit ? offset ?"
    rslt <- execute stmt 
        [
        (toSql (50 :: Integer)), (toSql (50 * (page-1))) 
        ]
    rows <- fetchAllRows' stmt
    return $ map sqlToAptRec rows

aptPutCommit :: Connection -> AptRecord -> IO Bool
aptPutCommit conn rec = do
    r <- aptPut conn rec
    commit conn
    return r

aptPut :: Connection -> AptRecord -> IO Bool
aptPut conn rec = do
    stmt <- prepare conn "insert or replace into apt(id, uri,title,price,address,neighborhood,lat,lng,mapuri,ws,ts,wsuri) values ( ?,?,?,?,?,?,?,?,?,?,?,? )"
    rslt <- execute stmt $ aptRecToSql rec
    finish stmt
    case rslt of
        1 -> return True
        _ -> return False

aptConnect :: AptDBHost -> AptDBUser -> AptDBPw -> AptDBType -> IO Connection
aptConnect host user pass typ =
    case (typ) of
        "sqlite" -> do
            conn <- connectSqlite3 host
            valid <- (handleSqlError $ aptValidateDB conn) `catch` (\e -> makeDB conn)
            case (valid) of 
                True -> return conn
                False -> ioError $ userError $ "unable to validate connection"
        _ -> ioError $ userError $ "unsupported db type '" ++ typ ++ "'"

aptValidateDB :: Connection -> IO Bool
aptValidateDB conn = do 
    getvsn <- (prepare conn "select version from schema_version")
    execute getvsn []
    rslt <- fetchAllRows' getvsn
    case (head . head $ rslt) of
        _ -> return True

makeDB :: Connection -> IO Bool
makeDB conn = do
    run conn "CREATE TABLE IF NOT EXISTS schema_version ( version integer )" []
    run conn "insert into schema_version (version) values (1)" []
    run conn "CREATE TABLE IF NOT EXISTS poi ( id integer primary key on conflict replace autoincrement, type varchar(255), address text , lat double, lng double )" []
    run conn "CREATE TABLE IF NOT EXISTS apt ( id integer primary key, uri text, title text, price integer, address text, neighborhood, lat double, lng double, mapuri text, ws integer, ts integer, wsuri text )" []
    commit conn
    return True
