module Server.Auth where 

import RIO
import Core
import Servant ((:>), (:<|>) (..))
import qualified Servant as S
import App (AppM)

type Authentication = "user" :> "login" :> S.ReqBody '[S.JSON] [User] :> S.Post '[S.JSON] User

type Registration = "user" :> S.ReqBody '[S.JSON] [User] :> S.Post '[S.JSON] User

type AuthAPI = Authentication :<|> Registration

auth = undefined

register = undefined

authAPI :: S.ServerT AuthAPI AppM
authAPI = auth :<|> register

