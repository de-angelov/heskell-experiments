module Lib where

import qualified Zero.Server as Server
import qualified Data.Text as T


helloHandler :: Server.Handler
helloHandler = Server.simpleHandler Server.GET "/hello" helloCallback

helloCallback :: Server.Request -> Server.Response
helloCallback _ = Server.stringResponse "hello" 

echoHandler :: Server.Request -> Server.Response
echoHandler req 
  = Server.stringResponse $ Server.requestBody req

caseHandler :: Server.Request -> Server.Response
caseHandler req = 
  Server.stringResponse result
  where
  result = 
    case Server.requestBody req of
      "1" -> "One"
      "2" -> "Two"
      _ -> "Three"

stringManipulationHandler :: Server.Request -> Server.Response
stringManipulationHandler req = 
  Server.stringResponse result
    where
    text = T.pack $ Server.requestBody req
    result = case T.isPrefixOf "I'm positive" $ text of
      True -> T.unpack $ T.replace "I'm positive" "I think" $ text
      False -> T.unpack text



run :: IO ()
run = Server.startServer 
  [ helloHandler 
  , Server.simpleHandler Server.POST "/echo" echoHandler
  , Server.simpleHandler Server.POST "/case" caseHandler
  , Server.simpleHandler Server.POST "/string-manipulation" stringManipulationHandler
  ]
