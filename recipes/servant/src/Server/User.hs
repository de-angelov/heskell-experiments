module Server.User where

import RIO
import Core
import Servant ((:>), (:<|>) (..))
import qualified Servant as S
import App (AppM, getDbConnection, AppEnv)
import Database.SQLite.Simple(Connection, query_)

type GetCurrent = "user" :> S.Get '[S.JSON] User

type UpdateUser = "user" :> S.ReqBody '[S.JSON] User :> S.Put '[S.JSON] User


getUser:: Connection -> IO [User]
getUser conn = query_ conn "SELECT * FROM user WHERE name = current"

import Database.SQLite.Simple(Connection, query_)
getCurrent :: AppM User
getCurrent = do 
  conn <- getDbConnection >>= liftIO 
  [user] <- liftIO $ query_ conn "SELECT * FROM users WHERE name = current"
  pure user



updateUser :: User -> AppM User
updateUser = undefined


type UserAPI
  = GetCurrent
  :<|> UpdateUser

userAPI :: S.ServerT UserAPI AppM
userAPI
  = getCurrent
  :<|> updateUser
