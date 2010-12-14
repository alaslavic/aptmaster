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
import "monads-fd" Control.Monad.Trans
import "monads-fd" Control.Monad.Reader
import AptData


main :: IO ()
main = do
    td <- newTemplateDirectory' "templates" $ bindStuff emptyTemplateState
    quickServer $ templateHandler td defaultReloadHandler $ \ts ->
        ifTop (render ts "apt") <|>
        --ifTop (writeBS "should be templates") <|>
        route [ ("foo", writeBS "bar")
              , ("echo/:echoparam", echoHandler)
              ] <|>
        dir "static" (fileServe ".")

bindStuff :: TemplateState Snap -> TemplateState Snap
bindStuff = 
    (bindSplice "results" resultsSplice)

resultsSplice :: Splice Snap
resultsSplice = do
    --input <- getParamNode 
    --return [X.Text $ B.pack $ "HelloWorld"]
    return [makeTable]
    where
        liftQuery = liftIO do 
            conn <- aptConnect "/tmp/foo.db" "" "" "sqlite"
            aptQuery conn 1
        makeTable = Element "table" [("id","results")] ( map (\x -> Element "tr" [] []) liftQuery )



echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
