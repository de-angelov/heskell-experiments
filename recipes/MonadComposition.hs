{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import RIO
import System.IO (putStrLn, getLine)
import Lens.Micro.Platform (makeLenses)
import RIO.State

data Env = Env { separator :: !Utf8Builder }

type StateIO = StateT Utf8Builder IO 

stateEnvExample :: ReaderT Env StateIO Utf8Builder
stateEnvExample = do 
  stateOld <- get
  envPrefix <- separator <$> ask 

  liftIO . putStrLn $ "Input a to change"

  let seperator = foldr (\_ acc -> acc <> envPrefix) "" [1..10]
  print  $ show . utf8BuilderToText $ seperator
  
  pure "End" 
  where 
    print :: String -> (ReaderT Env StateIO) ()
    print = liftIO . putStrLn 

main :: IO ()
main  = do 
  let env = Env { separator = "/--\\" }

  x <- evalStateT (runReaderT stateEnvExample env) "initial"

  pure ()
