{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server (startServer) where

import RIO
import Servant ((:<|>)(..) )
import qualified Servant as S

import Types (AppConfig (appPort))
import Pages.Home (HomePage, homePage)
import Pages.Login (LoginPage, loginPage)
import API.Auth (AuthAPI, authAPI)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import qualified Network.Wai.Handler.Warp as Warp

type ServerAPI
  = HomePage
  :<|> LoginPage
  :<|> AuthAPI

server :: HasLogFunc a => S.ServerT ServerAPI (RIO a)
server
  = homePage
  :<|> loginPage
  :<|> authAPI

apiProxy :: S.Proxy ServerAPI
apiProxy = S.Proxy


rioToHandler :: a -> RIO a b -> S.Handler b
rioToHandler env app = S.Handler $ ExceptT $ try $ runRIO env app

startServer :: AppConfig -> RIO a ()
startServer config =
  let 
    rioServer = S.hoistServer apiProxy $ rioToHandler config  
    app = S.serve apiProxy $ rioServer server
  in 
    Warp.defaultSettings
    & Warp.setPort (appPort config)
    & Warp.setTimeout 60
    & flip Warp.runSettings app 
    & liftIO 

-- startServer :: (HasLogFunc env, HasServantPort env) => RIO env ()
-- startServer = do
--   logInfo "Hello, Haskell from RIO StartServer"

--   port <- view servantPortL

--   run 8000 $ serve apiProxy server

-- runSettings :: Application -> Settings -> IO ()


