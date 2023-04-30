module Auth where 

import RIO 
import Core
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Control.Monad.Trans.Maybe ( MaybeT(runMaybeT) )
import qualified Network.Wai as Wai 

handleOptionalAuthentication :: AuthHandler Wai.Request (Maybe User)
handleOptionalAuthentication = undefined 


handleAuthentication  :: AuthHandler Wai.Request User 
handleAuthentication = undefined


getUserFromJwtToken :: Wai.Request -> IO (Maybe User)
getUserFromJwtToken req = undefined
