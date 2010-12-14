import CrawlerWorker
import AptData


main = do
    config <- getConfig "./crawler.json"
    links <- getCLLinks "http://sfbay.craigslist.org/sfc/apa/"
    conn <- aptConnect "/tmp/foo.db" "" "" "sqlite"
    let records = map fillCLRecord links
    let inserted = map (\r -> r >>= (aptPutCommit conn)) records
    printstuff $ zip links inserted



printstuff :: [(String,IO Bool)] -> IO ()
printstuff [] = return ()
printstuff ((link,action):xs) = do
    result <- action
    putStrLn (link ++ " Returned: " ++ (show result))
    printstuff xs
