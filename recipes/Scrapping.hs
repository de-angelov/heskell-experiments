{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Text.HTML.Scalpel (scrapeURL, attrs)
import Prelude (putStrLn, IO, Maybe (Just), mapM_, ($), String)

main :: IO ()
main =  do
    images <- scrapeURL "https://github.com/" $ attrs "src" "img"
    case images of 
        Just ar -> mapM_ putStrLn ar
        _ -> putStrLn "Error"
    putStrLn "End"

