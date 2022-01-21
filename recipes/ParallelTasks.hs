{-# LANGUAGE NoImplicitPrelude #-}

import Text.HTML.Scalpel (scrapeURL, attrs)
import Prelude (putStrLn, IO, Maybe (Just), mapM_, ($), String, Monad ((>>)), Int)
import System.Timeout(timeout)
import Control.Concurrent(threadDelay, forkIO)
import RIO (concurrently, race, Concurrently (runConcurrently), mapConcurrently, map, const)

magick :: IO ()
magick  =  do
  putStrLn "magick start"
  threadDelay 1000
  putStrLn "magick end"

main :: IO ()
main =  do
  x <- mapConcurrently (const magick) [1..10]

  putStrLn "End"

