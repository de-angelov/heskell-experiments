#!/usr/bin/env stack
-- stack script --resolver lts-18.18

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import RIO
import System.IO (putStrLn)

data AppSettings = AppSettings { appName :: !String }

class HasAppName env where 
  getAppName :: env -> String 

instance HasAppName AppSettings where 
  getAppName = appName 

class HasAppNameDirectGet env where 
  simpleGetAppName :: (MonadReader r env) => String 

type Env = RIO AppSettings ()
instance HasAppNameDirectGet Env where 
  simpleGetAppName = do
    env <- ask  
    appName env 


printAppNameShort :: HasAppNameDirectGet env => RIO env () 
printAppNameShort = 
  liftIO $ putStrLn $ "Print app name using 'simpleGetAppName' " <> simpleGetAppName


printAppNameLong :: HasAppName env => RIO env ()
printAppNameLong = do 
  env <- ask 
  let name = getAppName env 
  liftIO $ putStrLn $ "Print app name using 'getAppName': " <> name 

printAppNameRIO :: RIO AppSettings ()
printAppNameRIO = do 
  AppSettings name <- ask  
  liftIO $ putStrLn  $ "Print app name using 'ask': " <>  name


app :: RIO AppSettings () 
app = do 
  printAppNameRIO
  printAppNameLong
  liftIO . putStrLn $ "Lift IO SomeText"
 

main :: IO ()
main = do
  let env = AppSettings { appName = "Example App"}
  runRIO env app

