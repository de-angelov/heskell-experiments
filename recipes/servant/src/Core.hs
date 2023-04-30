{-# LANGUAGE DerivingVia #-}
module Core where 

import RIO 
import Data.Aeson
import Database.SQLite.Simple (FromRow)
import Database.SQLite.Simple.FromField (FromField)

newtype Tag = Tag Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON) 
  deriving FromField via Text 

newtype Date = Date Text 
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
  deriving FromField via Text  

newtype Email = Email Text 
  deriving (Eq, Show, Generic, FromJSON, ToJSON) 
  deriving FromField via Text  
  
data Comment 
  = Comment 
  { id :: Int 
  , createdAt :: Date
  , updatedAt :: Date
  , body :: Text 
  , author :: Profile
  } deriving (Eq, Show, Generic, FromJSON, ToJSON) 

data User 
  = User
  { email :: Email
  , token :: Text
  , username :: Text
  , bio :: Text
  , image :: Text
  } deriving (Eq, Show, Generic, FromJSON, ToJSON, FromRow)

data Article 
  = Article 
  { slug :: Text
  , title :: Text
  , description :: Text 
  , body :: Text
  , tagList :: [Tag]
  , createdAt :: Date
  , updatedAt :: Date
  , favorited :: Bool
  , favoritesCount :: Int 
  , author :: Profile
  } deriving (Eq, Show, Generic, FromJSON, ToJSON) 

data Profile
  = Profile 
  { username :: Text
  , bio :: Text 
  , image :: Text
  , following :: Bool
  } deriving (Eq, Show, Generic, FromJSON, ToJSON) 