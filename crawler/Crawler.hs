import CrawlerWorker


main = do
    config <- getConfig "./crawler.json"
    links <- getCLLinks "http://sfbay.craigslist.org/sfc/apa/"
    print links
