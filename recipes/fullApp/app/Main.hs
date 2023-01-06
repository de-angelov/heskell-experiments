module Main where

import qualified Cli
import RIO 
import qualified System.Log.Formatter as Logger
import qualified System.Log.Handler as Hangler
import qualified System.Log.Handler.Simple as Logger 

initLogger :: IO ()
initLogger = do 
    logger <- Logger.getRootLogger 
    handler <- Logger.streamHandler stdout Logger.Build
    let formatter = Logger.simpleLogFormatter "[$time : $loggername : $prio] $msg"

    Logger.saveGlobalLogger $ 
        logger 
            & Logger.setHandlers [Handler.setFormatter handler formatter]
            & Logger.setLevel Logger.INFO


main :: IO ()
main = do 
    initLogger 
    Cli.main 

    