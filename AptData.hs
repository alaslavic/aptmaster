module AptData
( AptRecord(AptRecord)
, AptWhere(And,Or,Grt,Les,Equ,Field,Value)
, AptOrder(Asc,Desc)
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
, aptPois
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
, aptExists
, aptQuery
, aptPoiPut
, aptPoiPutCommit
, aptPoiQuery
, aptPoiTypeQuery
)
where

import Database.HDBC
import Database.HDBC.Sqlite3
import Database.HDBC.MySQL
import Data.List
import qualified Data.Map as Map
import Control.Monad 
import Data.Maybe (maybe)

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
    , aptPois :: [(AptPoi,Double)]
    } deriving (Show)

data AptPoi = AptPoi
    { aptPoiId :: Int
    , aptPoiType :: String
    , aptPoiAddress :: String
    , aptPoiLat :: Double
    , aptPoiLong :: Double
    } deriving (Show)

data AptConnection = AptConnection
    { aptDB :: Connection
    }

type AptDBUser = String
type AptDBPw = String
type AptDBType = String
type AptDBHost = String
type AptDBInst = String

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
    [toSql $ aptPoiType rec
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
    , aptPois=[]
    }

sqlToAptPoi :: [SqlValue] -> AptPoi
sqlToAptPoi [id,typ,add,lat,lng] = AptPoi
    { aptPoiId=(fromSql id)
    , aptPoiType=(fromSql typ)
    , aptPoiAddress=(fromSql add)
    , aptPoiLat=(fromSql lat)
    , aptPoiLong=(fromSql lng)
    }

type AptPoiMap = Map.Map String [AptPoi]

data AptWhere = And | Or | Grt | Les | Equ | Field String | Value SqlValue deriving (Show)

data AptOrder = Asc String | Desc String deriving (Show)

makeWhere :: [String] -> [AptWhere] -> Maybe (String,[SqlValue])
makeWhere fields ws = 
    let
        m = foldl' ( collapse fields ) (Just ("",[])) ws
    in
        case m of 
            Just (x,y) -> if ((length x) > 0) then Just (" where " ++ x, y ) else Just (x,y)
            x -> x
    where
        collapse :: [String] -> Maybe (String,[SqlValue]) -> AptWhere -> Maybe (String,[SqlValue])
        collapse _ Nothing _ = Nothing
        collapse fields ( Just (wc,vs) ) And = Just (wc ++ " and ",vs)
        collapse fields ( Just (wc,vs) ) Or = Just (wc ++ " or ",vs)
        collapse fields ( Just (wc,vs) ) Grt = Just (wc ++ " > ",vs)
        collapse fields ( Just (wc,vs) ) Les = Just (wc ++ " < ",vs)
        collapse fields ( Just (wc,vs) ) Equ = Just (wc ++ " = ",vs)
        collapse fields ( Just (wc,vs) ) (Field x) = if (x `elem` fields) 
            then Just (wc ++ " ( " ++ x ,vs) 
            else Nothing
        collapse fields ( Just(wc,vs) ) (Value x) = Just( wc ++ " ? ) ", vs ++ [x] )

makeOrder :: [String] -> [AptOrder] -> String
makeOrder fields os = 
    let
        m = foldl ( collapse fields ) "" os
    in
        if ((length m) > 0 ) then " order by " ++ m else m
    where
        collapse :: [String] -> String -> AptOrder -> String
        collapse fields os (Asc f) = if ( f `elem` fields )
            then os ++ f ++ " Asc"
            else os
        collapse fields os (Desc f) = if ( f `elem` fields ) 
            then os ++ f ++ " Desc"
            else os

aptPoiQuery :: Connection -> [AptWhere] -> [AptOrder] -> IO [AptPoi]
aptPoiQuery conn w o= do
    let afields = ["address", "type", "lat", "lng"]
    let (wc,wv) = maybe ("",[]) id (makeWhere afields w)
    let oc = makeOrder afields o
    let q = "select * from poi " ++ wc ++ oc
    stmt <- prepare conn q
    execute stmt wv
    rows <- fetchAllRows' stmt
    return $ Data.List.map sqlToAptPoi rows

 
aptPoiQueryMap :: Connection -> IO AptPoiMap
aptPoiQueryMap conn = do
    ps <- aptPoiQuery conn [] []
    return $ foldr (\ p map -> Map.insertWith' (\ new old -> (head new):old ) (aptPoiType p) [p] map ) Map.empty ps

aptPoiTypeQuery :: Connection -> IO [String]
aptPoiTypeQuery conn = do
    stmt <- prepare conn "select distinct type from poi order by type asc"
    rslt <- execute stmt []
    rows <- fetchAllRows' stmt
    return $ Data.List.map (\r -> fromSql (head r) :: String ) rows

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

aptPoiAssoc :: Connection -> AptRecord -> (AptPoi,Double) -> IO Bool
aptPoiAssoc conn rec (poi,d) = do
    stmt <- prepare conn "insert or replace into apt_poi(aptid, poiid, type,dist) values(?,?,?,?)"
    rslt <- execute stmt [toSql $ aptId rec, toSql $ aptPoiId poi, toSql $ aptPoiType poi, toSql d]
    return $ case rslt of 
        1 -> True
        _ -> False

-- example query
--select apt.id, foo.dist, bar.dist, poi.type, apt_poi.dist from apt join apt_poi as foo on apt.id = foo.aptid and foo.type = "foo" join apt_poi as bar on apt.id = bar.aptid and bar.type = "bar" join apt_poi on apt.id = apt_poi.aptid join poi on apt_poi.poiid = poi.id order by foo.dist, bar.dist;
--  


aptExists :: Connection -> Int -> IO Bool
aptExists conn id = do
    stmt <- prepare conn "select count(id) from apt where id = ?"
    rslt <- execute stmt [toSql id]
    rows <- fetchAllRows' stmt
    return $ case (fromSql . head . head $ rows :: Int) of
        1 -> True
        _ -> False

aptQuery :: Connection -> [String] -> [AptWhere] -> [AptOrder] -> Int -> IO [AptRecord]
aptQuery conn ts ws os page = do
    let tfields = map (\x -> "min(" ++ x ++ ".dist) as " ++ x ++ "_dist" ) ts
    let tjoin = concat $ map (\t -> "left join apt_poi as " ++ t ++ " on apt.id = " ++ t ++ ".aptid and " ++ t ++ ".type = ? " ) ts
    let tvals = map toSql ts
    let fields = ["apt.id", "apt.uri", "apt.title", "apt.price", "apt.address", "apt.neighborhood", "apt.lat", "apt.lng", "apt.mapuri", "apt.ws", "apt.ts", "apt.wsuri" ]
    let ttext = concat $ intersperse "," ((map (\x -> drop 4 x) fields) ++ tfields)
    let s = "select " ++ ttext ++ " from apt " ++ tjoin ++ " group by apt.id limit ? offset ?"
    let poifields = ["poi.id", "poi.type", "poi.address", "poi.lat", "poi.lng", "apt_poi.dist"]
    let ftext = concat $ ( intersperse "," (fields ++ poifields) )
    let (wtext,wvals) = maybe ("",[]) (\x -> x) (makeWhere ( fields ++ ( map (\t -> "apt." ++ t ++ "_dist") ts ) ) ws)
    let otext = makeOrder ( fields ++ ( map (\t -> "apt." ++ t ++ "_dist") ts ) ) os
    let s' = "select " ++ ftext ++ " from ( " ++ s ++ " ) as apt left join apt_poi on apt.id = apt_poi.aptid left join poi on apt_poi.poiid = poi.id " ++ wtext ++ otext;
    let lv = [ (toSql (50 :: Integer)), (toSql (50 * (page-1))) ]
    putStrLn s'
    stmt <- prepare conn s'
    rslt <- execute stmt (tvals ++ lv ++ wvals )
    rows <- fetchAllRows' stmt
    --return $ Data.List.map sqlToAptRec rows
    return $ reverse $ snd $ foldr collapse (Nothing,[]) rows
    where
        collapse :: [SqlValue] -> (Maybe AptRecord,[AptRecord]) -> (Maybe AptRecord,[AptRecord])
        collapse row (current,rs) = 
            let
                (recf,poid) = splitAt 12 row
                (poif,ds) = splitAt 5 poid
                d = (fromSql $ head ds) :: Double
                rec = sqlToAptRec recf
                poi = sqlToAptPoi poif
            --in (Just ((sqlToAptRec recf){aptPois=[sqlToAptPoi), rs)
            in case current of 
                Nothing -> ( Just rec{ aptPois=[(poi,d)] }, rs )
                Just c -> if ((aptId c) == (aptId rec))
                    then ( Just c{ aptPois=((poi,d):(aptPois c)) }, rs )
                    else ( Just rec{ aptPois=[(poi,d)] }, (c{aptPois=(reverse $ aptPois c)}):rs )

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
    rslts <- mapM (aptPoiAssoc conn rec) $ aptPois rec
    case rslt of
        1 -> return True
        _ -> return False

aptConnect :: AptDBHost -> AptDBInst-> AptDBUser -> AptDBPw -> AptDBType -> IO Connection
aptConnect host inst user pass typ =
    case (typ) of
        "sqlite" -> do
            conn <- connectSqlite3 host
            valid <- (handleSqlError $ aptValidateDB conn) `catch` (\e -> makeDB conn)
            case (valid) of 
                True -> return conn
                False -> ioError $ userError $ "unable to validate connection"
        "mysql" -> do
            conn <- connectMySQL defaultMySQLConnectionInfo { mysqlHost=host, mysqlUser=user,mysqlPassword=pass,mysqlDatabase=inst }
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
    run conn "CREATE TABLE IF NOT EXISTS apt_poi ( aptid integer, poiid integer, type varchar(255), dist double )" []
    run conn "CREATE UNIQUE INDEX apt_poi_uniq on apt_poi( aptid, poiid, type )"  []
    commit conn
    return True
