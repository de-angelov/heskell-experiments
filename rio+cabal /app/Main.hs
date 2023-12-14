{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import RIO
import Prelude (putStrLn)
import Server(startServer)
import Types(AppConfig(..))




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

main :: IO ()
main = 
  putStrLn "Hello, Haskell from IO"

  >> logOptionsHandle stderr False 

  >>= pure  
  . setLogUseTime True 
  . setLogMinLevel LevelDebug 
  . setLogUseLoc True 
  . setLogUseColor True   

  >>= \logOptions -> withLogFunc logOptions 

  $ \logFunc ->   
  let 
    appConfig 
          = AppConfig 
          { appLogFunc = logFunc
          , appPort = 8001 
          }
          
  in runRIO appConfig 
  $ logInfo "Hello, Haskell from RIO"
  >> startServer appConfig 