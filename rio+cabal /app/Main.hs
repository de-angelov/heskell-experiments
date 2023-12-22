{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import RIO
import Prelude (putStrLn)
import Server(startServer)
import Types(AppConfig(..))
import qualified DB.Transaction as DBT




-- main = do
-- putStrLn "Hello, Haskell from IO"
-- logOptions' <- logOptionsHandle stderr False
-- let logOptions 
--       =  logOptions'
--       & setLogUseTime True  
--       & setLogMinLevel LevelDebug 
--       & setLogUseLoc True
--       & setLogUseColor True
-- withLogFunc logOptions $ do
--   let appConfig 
--           = AppConfig 
--           { appLogFunc = logFunc
--           , appPort = 8001 
--           }
--   runRIO appConfig $ logInfo "Hello, Haskell from RIO" >> startServer 

getDBPool = 
    let 
      connectionString = undefined 
      poolSize = 4 
    in DBT.loadPool connectionString poolSize

main :: IO ()
main = 
  putStrLn "Hello, Haskell from IO"
  >>  logOptionsHandle stderr False 
  >>= pure  
  . setLogUseTime True 
  . setLogMinLevel LevelDebug 
  . setLogUseLoc True 
  . setLogUseColor True   
  >>= \logOptions -> withLogFunc logOptions 


  $ \logFunc -> getDBPool
  >>= \dbPool ->
  
  let 
    appConfig 
          = AppConfig 
          { appLogFunc = logFunc
          , appPort = 8001 
          , appDBPool = dbPool -- todo
          }
          
  in runRIO appConfig 
  $ logInfo "Hello, Haskell from RIO"
  >> startServer appConfig 