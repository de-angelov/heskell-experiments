{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Crawler 
  (startCrawler
  ,CrawlerConfig(..)
  ) where 

import RIO 
import RIO.Text (intercalate)
import Text.HTML.Scalpel ( scrapeURL, attrs, URL, texts ) 
import StringUtils ( Print(makeString) )
import Prelude (putStrLn)

data CrawlerConfig = Config 
  { configSites :: ![Utf8Builder]
  } 

data InnerConfig = InnerConfig 
  { appLogFunc :: !LogFunc 
  , config :: !CrawlerConfig
  }

instance HasLogFunc InnerConfig where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

startCrawler :: CrawlerConfig -> IO [Utf8Builder]
startCrawler outerSettings = do
  -- Loging settings

  logOptions' <- logOptionsHandle stderr False
  let logOptions 
        =  logOptions'
        & setLogUseTime True  
        & setLogMinLevel LevelDebug 
        & setLogUseLoc True
        & setLogUseColor True

  withLogFunc logOptions $ \logFunc -> do
    let appInnerSettings = InnerConfig
          { appLogFunc = logFunc
          , config = outerSettings
          }

    runRIO appInnerSettings $ do 

      sitesToCrawl <- configSites <$> config <$> ask 

      let
        scrape (url :: URL) = scrapeURL url $ attrs "src" "img"
        getScrapeResults = mapM (scrape . makeString) sitesToCrawl  :: IO [Maybe [Text]]

      x <- sequence <$> liftIO getScrapeResults 

      let result = 
            case x of
            Nothing -> [] :: [Utf8Builder]
            Just y ->  y & join & map display

      pure result

