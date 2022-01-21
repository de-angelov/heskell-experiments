{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import RIO
import System.IO (putStrLn, getLine)
import Lens.Micro.Platform (makeLenses)
import RIO.State


stateExample :: StateT Utf8Builder IO Utf8Builder
stateExample = do 
  old <- get 
  lift . putStrLn $ "Input a to change"

  new <- lift $ displayShow <$> getLine 
  put $ new <> old

  pure $ new <> " " <> old

        
main :: IO ()
main  = do  
  y <- show . utf8BuilderToText <$> (execStateT stateExample "2 initial 2") 
  putStrLn  y

  pure ()
