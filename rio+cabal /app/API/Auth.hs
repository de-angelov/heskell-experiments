{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module API.Auth where 

import RIO 
import Servant ((:>), (:<|>) (..))
import qualified Servant as S
import Types (User, AppConfig)

type Authentication 
  = "api" :> "login" 
  :> S.ReqBody '[S.JSON] [User] 
  :> S.Post '[S.JSON] User

type Registration 
  = "api" :> "register" 
  :> S.ReqBody '[S.JSON] [User]
  :> S.Post '[S.JSON] User 

type AuthAPI = Authentication :<|> Registration 

login = undefined 


register = undefined 

authAPI :: S.ServerT AuthAPI (RIO a) 
authAPI = login :<|> register

