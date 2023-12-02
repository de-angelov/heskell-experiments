module Piece 
  ( main
  , runServer
  , mkAppEnv ) where 

import CakeSlayer.Db (initialisePool)

import Piece.App.Env (Env (..))
import Piece.App.Monad (AppEnv, runApp)
import Piece.Db.Schema (prepareDb)
import Piece.Server (PieceApi, server)
import Piece.Effects.Log (mainLogAction)
import Colog (Severity (Debug))
import Servant (serve)
import Network.Wai.Handler.Warp (run)

mkAppEnv :: IO AppEnv 
mkAppEnv = do 
  -- IO Configuration 
  envDbPool <- initialisePool 
    "host=localhost port=5432 user=postgres password=postgres dbname=postgres"
  
  -- pure configuration 
  let envLogAction = mainLogAction Debug 
  pure Env{..}


runServer :: AppEnv -> IO ()
runServer env = 
  let application = serve (Proxy @PieceApi) (server env)
  in runApp env prepareDb >> run 8080 application  

main:: IO ()
main = do 
  putStrLn "main Hello, Haskell!"
  mkAppEnv >>= runServer