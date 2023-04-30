module Server.Article where 

import RIO
import Core
import qualified Servant as S
import Servant ((:>), (:<|>) (..))
  
type ListArticles 
  = "articles" 
  :> S.QueryParam "tag" Text
  :> S.QueryParam "author" Text
  :> S.QueryParam "favorited" Text
  :> S.QueryParam "limit" Text
  :> S.QueryParam "offset" Text
  :> S.Get '[S.JSON] [Article]

type FeedArticles 
  = "articles"
  :> S.QueryParam "limit" Text
  :> S.QueryParam "offset" Text
  :> S.Get '[S.JSON] [Article]

type GetArticle 
  = "articles"
  :> S.Capture "slug" Text
  :> S.Get '[S.JSON] Article  

type CreateArticle 
  = "articles"
  :> S.ReqBody '[S.JSON] Article 
  :> S.Post '[S.JSON] Article 

type UpdateArticle 
  = "articles"
  :> S.Capture "slug" Text
  :> S.Put '[S.JSON] Article  

type DeleteArticle 
  = "articles"
  :> S.Capture "slug" Text
  :> S.Delete '[S.JSON] Article 

type FavoriteArticle 
  = "article"
  :> S.Capture "slug" Text
  :> "favorite"
  :> S.Post '[S.JSON] Article 

type UnFavoriteArticle 
  = "article"
  :> S.Capture "slug" Text
  :> "favorite"
  :> S.Delete '[S.JSON] Article 


listArticles = undefined

feedArticles = undefined

getArticle = undefined

createArticle = undefined

updateArticle = undefined

deleteArticle = undefined

favoriteArticle = undefined

unFavoriteArticle = undefined

type ArticleAPI
  = ListArticles
  :<|> FeedArticles
  :<|> GetArticle
  :<|> CreateArticle
  :<|> UpdateArticle
  :<|> DeleteArticle
  :<|> FavoriteArticle
  :<|> UnFavoriteArticle

articleAPI :: S.Server ArticleAPI
articleAPI 
  = listArticles
  :<|> feedArticles
  :<|> getArticle 
  :<|> createArticle
  :<|> updateArticle
  :<|> deleteArticle
  :<|> favoriteArticle
  :<|> unFavoriteArticle



