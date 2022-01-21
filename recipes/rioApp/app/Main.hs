{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import RIO 
import Prelude (print)
import Crawler (startCrawler, CrawlerConfig(Config) )
import RIO.Text (intercalate)
import Data.Yaml.Config (loadYamlSettings, ignoreEnv)

import Data.Yaml (FromJSON, decodeEither,  decodeEither', decodeThrow)

data Env = Env { targetSites:: ![Text] } 
  deriving (Show, Generic) 

instance FromJSON Env

main :: IO ()
main = do
  content <- readFileBinary  "./config.yaml"

  let 
    settings =
          case (decodeThrow  content :: Maybe Env ) of 
          Nothing -> [] :: [Text]
          Just env -> targetSites env 
    appConfig = Config $ map display  settings

  result <- startCrawler appConfig  
  
  result 
    & map textDisplay 
    & intercalate ", " 
    & print 

  pure ()
  
