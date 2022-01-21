#!/usr/bin/env stack
-- stack script --resolver lts-18.18

{-# LANGUAGE NoImplicitPrelude #-}

import Text.HTML.Scalpel (scrapeURL, attrs)
import Prelude (putStrLn, IO, Maybe (Just), mapM_, ($), String, Monad ((>>)), Int)
import System.Timeout(timeout)
import Control.Concurrent(threadDelay, forkIO)
import RIO (concurrently)

magick :: IO ()
magick =  do
  putStrLn "magick start"
  threadDelay 1000
  putStrLn "magick end"

main :: IO ()
main =  do
  _ <- concurrently magick magick

  putStrLn "End"

