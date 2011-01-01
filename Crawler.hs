import CrawlerWorker
import AptData
import Control.Monad
--import Control.Monad.State
import System.Console.GetOpt
import qualified Data.Map as Map
import System (getArgs)
import Data.Maybe


options :: [OptDescr (String,String)]
options = 
    [ Option "v" ["verbose"] (NoArg ("verbose", "")) "verbose output"
    , Option "h" ["help"] (NoArg ("help", "")) "this menu"
    , Option "d" ["dbhost"] (ReqArg (\x -> ("dbhost", x)) "Hostname|Filename" ) "database host ( or filename for sqlite)"
    , Option "i" ["dbinst"] (ReqArg (\x -> ("dbinst", x)) "<DB>" ) "database instance"
    , Option "u" ["dbuser"] (ReqArg (\x -> ("dbuser", x)) "Username" ) "database user"
    , Option "p" ["dbpass"] (ReqArg (\x -> ("dbpass", x)) "Password" ) "database password"
    , Option "t" ["dbtype"] (ReqArg (\x -> ("dbtype", x)) "sqlite|mysql|postgres" ) "database type"
    , Option "c" ["clpage"] (ReqArg (\x -> ("clpage", x)) "URL" ) "the craigslist apartment index page you want to index" 
    ]

parseOptions :: [String] -> IO (Map.Map String String, [String])
parseOptions argv = case getOpt Permute options argv of
    (o,n,[]) -> return (Map.fromList o,n)
    (_,_,er) -> ioError (userError (concat er ++ usageInfo "Usage:\n" options ) ) 


--data CrawlerState = CrawlerState
--    { conn :: Connection
--    , config :: CrawlerConfig
--    } deriving (Show)
--
--type CrawlerST = State CrawlerState
--

main = do
    argv <- getArgs
    (config,leftover) <- parseOptions argv
    if (Map.member "help" config) then ioError $ userError $ usageInfo "Usage:\n" options else return ()
    case leftover of 
        [] -> do 
            crawled <- crawl config
            printstuff crawled
        ("crawl":_) -> do
            crawled <- crawl config
            printstuff crawled
        ["add", "poi", typ, address] -> do
            poi <- addPoi config typ address
            putStrLn $ show poi
        _ -> do ioError $ userError $ usageInfo "Usage:\n" options



printstuff :: [(String,IO Bool)] -> IO ()
printstuff [] = return ()
printstuff ((link,action):xs) = do
    result <- action `catch` (\e -> return False)
    putStrLn (link ++ " Returned: " ++ (show result))
    printstuff xs
