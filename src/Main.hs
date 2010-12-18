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
import Data.List
import AptData
import Text.HJson
import Data.Map as Map


main :: IO ()
main = do
    td <- newTemplateDirectory' "templates" $ bindStuff emptyTemplateState
    quickServer $ templateHandler td defaultReloadHandler $ \ts ->
        ifTop (render ts "apt") <|>
        --ifTop (writeBS "should be templates") <|>
        route [ ("foo", writeBS "bar")
              , ("results/json", resultsHandlerJson)
              , ("echo/:echoparam", echoHandler)
              ] <|>
        dir "static" (fileServe ".")

bindStuff :: TemplateState Snap -> TemplateState Snap
bindStuff = 
    (bindSplice "results" resultsSplice)

resultsHandlerJson :: Snap ()
resultsHandlerJson = do
    results <- liftIO query
    writeBS $ B.pack $ toString $ JArray $ Data.List.map makeObj results
    where
        makeObj :: AptRecord -> Json
        makeObj rec = JObject $ Map.fromList $ Data.List.zip fields $ values rec
        query :: IO [AptRecord]
        query = do
            conn <- aptConnect "/tmp/foo.db" "" "" "sqlite"
            aptQuery conn 1
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
            , "wsuri" ]
        values :: AptRecord -> [Json]
        values rec =
            [ JNumber $  aptId rec
            , JString $  aptUri rec
            , JString $  aptTitle rec
            , JNumber $  aptPrice rec
            , JString $  aptAddress rec
            , JString $  aptNeighborhood rec
            , JNumber $ toRational $  aptLat rec
            , JNumber $ toRational $ aptLong rec
            , JString $  aptMapuri rec
            , JNumber $  aptWalkscore rec
            , JNumber $  aptTranscore rec
            , JString $  aptWsuri rec ]

resultsSplice :: Splice Snap
resultsSplice = do
    --input <- getParamNode 
    --return [Element "table" [("id","foo")] [Text "junk"]]
    liftIO makeTable
    where
        query :: IO [AptRecord]
        query = do 
            conn <- aptConnect "/tmp/foo.db" "" "" "sqlite"
            aptQuery conn 1
        makeTable :: IO [NodeG [] ByteString ByteString]
        makeTable = do
            results <- query
            rows <- makeRows results
            let th = Element "tr" [] $ Data.List.map hcol headers
            return [Element "table" [("id","results")] (th:rows)]
        makeRows :: [AptRecord] -> IO [NodeG [] ByteString ByteString]
        makeRows rs = do
            return $ Data.List.map (\x -> Element "tr" [] (makeRow x)) rs
        makeRow :: AptRecord -> [NodeG [] ByteString ByteString]
        makeRow rec = Data.List.map (\x -> col x ) $ columns rec
        col :: String -> NodeG [] ByteString ByteString
        col t = Element "td" [] [Text (B.pack t)]
        hcol :: String -> NodeG [] ByteString ByteString
        hcol t = Element "th" [] [Text (B.pack t)]
        headers = 
            [ "Id"
            , "Uri"
            , "Title"
            , "Price"
            , "Address"
            , "Neighborhood"
            , "Lat"
            , "Long"
            , "Mapuri"
            , "Walkscore"
            , "Transcore"
            , "Wsuri" ]
        columns rec =
            [ show $  aptId rec
            , show $  aptUri rec
            , show $  aptTitle rec
            , show $  aptPrice rec
            , show $  aptAddress rec
            , show $  aptNeighborhood rec
            , show $  aptLat rec
            , show $  aptLong rec
            , show $  aptMapuri rec
            , show $  aptWalkscore rec
            , show $  aptTranscore rec
            , show $  aptWsuri rec ]


echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
