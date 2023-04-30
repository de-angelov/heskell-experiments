module Server.Tag where 
  
import RIO
import Core
import qualified Servant as S
import Servant ((:>), (:<|>) (..))

type GetTags 
  = "tags"
  :> S.Get '[S.JSON] [Tag]

getTags = undefined

type TagAPI = GetTags

tagAPI :: S.Server TagAPI
tagAPI = getTags