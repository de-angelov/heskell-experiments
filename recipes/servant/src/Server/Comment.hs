module Server.Comment where

import RIO
import Core
import Servant ((:>), (:<|>) (..))
import qualified Servant as S

type GetCommentsFromArticle 
  = "articles"
  :> S.Capture "slug" Text
  :> "comments"
  :> S.Capture "id" Int 
  :> S.Get '[S.JSON] [Comment]

type DeleteComment
  = "articles"
  :> S.Capture "slug" Text
  :> "comments"
  :> S.Capture "id" Int 
  :> S.Delete '[S.JSON] Comment

type AddComment 
  = "articles"
  :> S.Capture "slug" Text
  :> "comments"
  :> S.Post '[S.JSON] Comment

type UpdateComment 
  = "aticles"
  :> S.Capture "slug" Text
  :> "comments"
  :> S.Capture "id" Int 
  :> S.Put '[S.JSON] Comment

type CommentAPI 
  = GetCommentsFromArticle 
  :<|> AddComment
  :<|> UpdateComment 
  :<|> DeleteComment 


addComment = undefined

getCommentsFromArticle = undefined

deleteComment = undefined

updateComment = undefined


commentAPI :: S.Server CommentAPI
commentAPI 
  = getCommentsFromArticle 
  :<|> addComment 
  :<|> updateComment 
  :<|> deleteComment