module Server(startApp) where 

import RIO 
import RIO.Text(unpack)
import Data.Aeson
import Core
import qualified Servant as S
import Servant.Server.Experimental.Auth as SAuth
import Servant ((:>), (:<|>) (..))
import Prelude(putStrLn)
import qualified Network.Wai.Handler.Warp as Warp 
import qualified Db as DB 
import Server.User 
import Server.Article
import Server.Tag
import Server.Comment
import Server.Auth
import Server.Profile
import App(AppEnv(..), AppM, runHandler)
import qualified Servant.Server.Experimental.Auth as SAuth
import Network.Wai (Request)

type AppAPI 
  = "api" 
  :> ( AuthAPI 
  :<|> UserAPI 
  -- :<|> ArticleAPI
  -- :<|> CommentAPI
  -- :<|> ProfileAPI
  -- :<|> TagAPI
  )

server :: S.ServerT AppAPI AppM
server 
  = authAPI 
  :<|> userAPI 
  -- :<|> articleAPI 
  -- :<|> commentAPI
  -- :<|> profileAPI
  -- :<|> tagAPI

apiProxy :: S.Proxy AppAPI
apiProxy = S.Proxy 

authProxy :: S.Proxy AuthContext
authProxy = S.Proxy

type AuthContext = SAuth.AuthHandler Request User ': SAuth.AuthHandler Request (Maybe User) ': '[]


app ::  AppEnv -> S.Application
-- app env = S.serve apiProxy server
app env = S.serveWithContext apiProxy serverAuthContext hoistedServer
  where 
    serverAuthContext :: S.Context AuthContext
    serverAuthContext = undefined

    hoistedServer = S.hoistServerWithContext apiProxy authProxy (runHandler env) server


startApp :: IO ()
startApp = do 
  let 
    dbFile = "./ExampleAPIApp.db";
    warpSettings 
      = Warp.defaultSettings
      & Warp.setPort 300
      & Warp.setTimeout 60
    env = AppEnv { dbFile = dbFile }

  DB.init dbFile

  
  putStrLn "server started at port 3000"
  Warp.runSettings warpSettings $ app env

