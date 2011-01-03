{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Types
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.Templating.Heist.TemplateDirectory
import           Text.XML.Expat.Tree as X
import           Data.ByteString.Char8 as B

import           Glue
import           Server


import Control.Monad
import Control.Monad.CatchIO
--import "monads-fd" Control.Monad.Trans
--import "monads-fd" Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.List as L
import AptData
import Text.HJson
import Data.Map as Map
import System.Console.GetOpt
import System (getArgs)
import Database.HDBC
--import Database.HDBC.Sqlite3
import Database.HDBC.MySQL
import Text.Regex.PCRE as PCRE


options :: [OptDescr (String,String)]
options = 
    [ Option "v" ["verbose"] (NoArg ("verbose", "")) "verbose output"
    , Option "h" ["help"] (NoArg ("help", "")) "this menu"
    , Option "d" ["dbhost"] (ReqArg (\x -> ("dbhost", x)) "Hostname|Filename" ) "database host ( or filename for sqlite)"
    , Option "i" ["dbinst"] (ReqArg (\x -> ("dbinst", x)) "<DB>" ) "database instance"
    , Option "u" ["dbuser"] (ReqArg (\x -> ("dbuser", x)) "Username" ) "database user"
    , Option "p" ["dbpass"] (ReqArg (\x -> ("dbpass", x)) "Password" ) "database password"
    , Option "t" ["dbtype"] (ReqArg (\x -> ("dbtype", x)) "sqlite|mysql|postgres" ) "database type"
    , Option "P" ["port"] (ReqArg (\x -> ("port", x)) "PORT" ) "tcp port to listen on"
    ]

parseOptions :: [String] -> IO (Map.Map String String, [String])
parseOptions argv = case getOpt Permute options argv of
    (o,n,[]) -> return (Map.fromList o,n)
    (_,_,er) -> ioError (userError (L.concat er ++ usageInfo "Usage:\n" options ) ) 

data AppState = AppState 
    { appConfig :: Map.Map String String
    , appConn :: Connection
    }


main :: IO ()
main = do
    args <- getArgs
    (config,leftover) <- parseOptions args
    if (Map.member "help" config) then ioError $ userError $ usageInfo "Usage:\n" options else return ()
    let h = Map.findWithDefault "localhost" "dbhost" config
    let u = Map.findWithDefault "" "dbuser" config
    let p = Map.findWithDefault "" "dbpass" config
    let t = Map.findWithDefault "mysql" "dbtype" config
    let i = Map.findWithDefault "test" "dbinst" config
    conn <- aptConnect h i u p t
    let state = AppState{ appConfig=config, appConn=conn }
    --td <- newTemplateDirectory' "templates" $ bindStuff emptyTemplateState
    td <- newTemplateDirectory' "templates" $ emptyTemplateState
    quickServer config $ templateHandler td defaultReloadHandler $ \ts ->
        --ifTop (render ts "apt") <|>
        ifTop (fileServeSingle "html/index.html") <|>
        --ifTop (writeBS "should be templates") <|>
        route [ ("foo", writeBS "bar")
              , ("results/json", (resultsHandlerJson state))
              , ("poi/json", (poiHandlerJson state))
              , ("echo/:echoparam", echoHandler)
              ] <|>
        dir "static" (fileServe "static")

--bindStuff :: TemplateState Snap -> TemplateState Snap
--bindStuff = 
--    (bindSplice "results" resultsSplice)

poiHandlerJson :: AppState -> Snap ()
poiHandlerJson state = do
    let conn = appConn state
    ps <- liftIO $ aptPoiQuery conn [] []
    writeBS $ B.pack $ toString $ JArray $ L.map makePoi ps
    where
        makePoi :: AptPoi -> Json
        makePoi p = 
            let
                fields = ["id","type","address","lat","lng"]
                values = L.map (\x -> x p) $ [ n . aptPoiId, JString . aptPoiType, JString . aptPoiAddress, n . aptPoiLat, n . aptPoiLong ]
            in JObject $ Map.fromList $ L.zip fields values
            where
                n :: ( Num a, Real a ) => a -> Json
                n = JNumber . toRational

resultsHandlerJson :: AppState -> Snap ()
resultsHandlerJson state = do
    r <- getRequest
    let conn = appConn state
    ts <- liftIO $ aptPoiTypeQuery conn
    let tsfs = L.map (\x -> x ++ "_dist") ts
    pagef <- (getParam "page") >>= (\p -> return $ B.unpack $ maybe "1" id p )
    let page = if ( pagef =~ ("^\\d+$" :: String) ) then ( read pagef ::Int ) else 1
    liftIO $ Prelude.putStrLn (show $ rqParam "filter" r)
    let filter = L.foldr wFold [] $ maybe [] id $ rqParam "filter" r 
    liftIO $ Prelude.putStrLn (show filter)
    let order = L.foldr oFold [] $ maybe [] id $ rqParam "order" r
    liftIO $ Prelude.putStrLn (show order)
    results <- liftIO $ aptQuery conn ts filter order page
    liftIO $ rollback conn
    liftIO $ Prelude.putStrLn $ "first: " ++ (show $ L.head results)
    liftIO $ Prelude.putStrLn $ "last: " ++ (show $ L.last results)
    writeBS $ B.pack $ toString $ JArray $ L.map makeObj results
    where
        oFold :: ByteString -> [AptOrder] -> [AptOrder]
        oFold order o = case ((B.unpack order) =~ ("^(asc|desc)(.+)$" :: String) :: [[String]] ) of
            [[_,"asc",x]] -> (Asc x):o
            [[_,"desc",x]] -> (Desc x):o
            _ -> o
        wFold :: ByteString -> [AptWhere] -> [AptWhere]
        wFold filter w = case ((B.unpack filter) =~ ("([^<>=]*)([<>=])(.*)" :: String) :: [[String]] ) of
            [[_,f,o,v]] -> if ( f =~ ("dist$" :: String) :: Bool ) 
                then case ( mkDouble v ) of
                    Nothing -> w
                    Just x -> [Field f, op o, Value $ toSql x] ++ (wAnd w)
                else case ( mkInt v ) of
                    Nothing -> w
                    Just x -> [Field f, op o, Value $ toSql x] ++ (wAnd w)
            _ -> w
        wAnd :: [AptWhere] -> [AptWhere]
        wAnd [] = []
        wAnd xs = And:xs
        mkInt :: String -> Maybe Int
        mkInt s = if ( s =~ ("^\\d+$" :: String) :: Bool)
            then Just (read s :: Int)
            else Nothing
        mkDouble :: String -> Maybe Double
        mkDouble s = if ( s =~ ("^\\d+\\.\\d+$" :: String) :: Bool)
            then Just (read s :: Double)
            else Nothing
        op :: String -> AptWhere
        op ">" = Grt
        op "<" = Les
        op "=" = Equ
        makeObj :: AptRecord -> Json
        makeObj rec = JObject $ Map.fromList $ L.zip fields $ values rec
        fields = 
            [ "id"
            , "uri"
            , "title"
            , "price"
            , "address"
            , "neighborhood"
            , "lat"
            , "lng"
            , "mapuri"
            , "ws"
            , "ts"
            , "wsuri"
            , "poi_dist" ]
        values :: AptRecord -> [Json]
        values rec =
            [ JNumber $ toRational $ aptId rec
            , JString $  aptUri rec
            , JString $  aptTitle rec
            , JNumber $  toRational $ aptPrice rec
            , jStringOrNull $  aptAddress rec
            , jStringOrNull $  aptNeighborhood rec
            , jNumOrNull $  aptLat rec
            , jNumOrNull $ aptLong rec
            , jStringOrNull $  aptMapuri rec
            , jNumOrNull $ aptWalkscore rec
            , jNumOrNull $ aptTranscore rec
            , jStringOrNull $  aptWsuri rec
            , makePois $ aptPois rec ]
        makePois :: [(AptPoi,Double)] -> Json
        makePois = JObject . (Map.mapMaybe (\x -> Just $ toJson x)) . (L.foldr pFold Map.empty)
        pFold :: (AptPoi,Double) -> Map String Double -> Map String Double
        pFold (p,d') map = 
            let 
                t = aptPoiType p
                d = maybe 1000000.0 id $ Map.lookup t map
            in if ( d' < d ) 
                then Map.insert t d' map
                else map
        jStringOrNull :: Maybe String -> Json
        jStringOrNull (Just s) = JString s
        jStringOrNull Nothing = JNull
        jNumOrNull :: ( Num a, Real a ) => Maybe a -> Json
        jNumOrNull Nothing = JNull
        jNumOrNull (Just n) = JNumber $ toRational n

--resultsSplice :: Splice Snap
--resultsSplice = do
--    --input <- getParamNode 
--    --return [Element "table" [("id","foo")] [Text "junk"]]
--    liftIO makeTable
--    where
--        query :: IO [AptRecord]
--        query = do 
--            conn <- aptConnect "/tmp/foo.db" "" "" "sqlite"
--            aptQuery conn [] [] [] 1
--        makeTable :: IO [NodeG [] ByteString ByteString]
--        makeTable = do
--            results <- query
--            rows <- makeRows results
--            let th = Element "tr" [] $ L.map hcol headers
--            return [Element "table" [("id","results")] (th:rows)]
--        makeRows :: [AptRecord] -> IO [NodeG [] ByteString ByteString]
--        makeRows rs = do
--            return $ L.map (\x -> Element "tr" [] (makeRow x)) rs
--        makeRow :: AptRecord -> [NodeG [] ByteString ByteString]
--        makeRow rec = L.map (\x -> col x ) $ columns rec
--        col :: String -> NodeG [] ByteString ByteString
--        col t = Element "td" [] [Text (B.pack t)]
--        hcol :: String -> NodeG [] ByteString ByteString
--        hcol t = Element "th" [] [Text (B.pack t)]
--        headers = 
--            [ "Id"
--            , "Uri"
--            , "Title"
--            , "Price"
--            , "Address"
--            , "Neighborhood"
--            , "Lat"
--            , "Long"
--            , "Mapuri"
--            , "Walkscore"
--            , "Transcore"
--            , "Wsuri" ]
--        columns rec =
--            [ show $  aptId rec
--            , show $  aptUri rec
--            , show $  aptTitle rec
--            , show $  aptPrice rec
--            , show $  aptAddress rec
--            , show $  aptNeighborhood rec
--            , show $  aptLat rec
--            , show $  aptLong rec
--            , show $  aptMapuri rec
--            , show $  aptWalkscore rec
--            , show $  aptTranscore rec
--            , show $  aptWsuri rec ]


echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
