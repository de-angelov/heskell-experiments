module Server.Profile where

import RIO 
import Data.Aeson
import Core
import qualified Servant as S
import Servant ((:>), (:<|>) (..))

unfollowUser = undefined

getUserProfile = undefined 

followUser = undefined

type GetUserProfile = "profiles" :> S.Capture "username" Text :> S.Get '[S.JSON] Profile

type FollowUser = "profiles" :> S.Capture "username" Text :> "follow" :> S.Post '[S.JSON] Profile

type UnFollowUser = "profiles" :> S.Capture "username" Text :> "follow" :> S.Delete '[S.JSON] Profile

type ProfileAPI 
  = GetUserProfile
  :<|> FollowUser
  :<|> UnFollowUser

profileAPI :: S.Server ProfileAPI 
profileAPI 
  =  getUserProfile 
  :<|> followUser 
  :<|> unfollowUser