module Lib where

import qualified Zero.Server as Server

helloHandler :: Server.Handler
helloHandler = Server.simpleHandler Server.GET "/hello" helloCallback

helloCallback :: Server.Request -> Server.Response
helloCallback req = Server.stringResponse "hello" 

run :: IO ()
run = Server.startServer [ helloHandler ]
