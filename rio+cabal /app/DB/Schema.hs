{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DB.Schema where 

import RIO 
import qualified Rel8 as R
import Data.Aeson.Types (ToJSON,FromJSON)
import Types (User(..), createUserPassword, createUser)

newtype UserId = UserId Text 
  deriving stock (Generic) 
  deriving newtype (Eq, Show, Read, FromJSON, ToJSON, R.DBEq, R.DBType)

newtype HashedPassword = HashedPassword Text
  deriving stock (Generic) 
  deriving newtype (Eq, Show, Read, FromJSON, ToJSON, R.DBEq, R.DBType)

data UserEntity f 
  = UserEntity 
  { entityUserId :: R.Column f UserId 
  , entityUserName :: R.Column f Text
  , entityUserPassword :: R.Column f HashedPassword 
  } deriving stock (Generic) 
    deriving anyclass (R.Rel8able)


userSchema :: R.TableSchema (UserEntity R.Name)
userSchema 
  = R.TableSchema
  { R.name = "users"
  , R.schema = Nothing 
  , R.columns 
    = UserEntity 
    { entityUserId = "user_id"
    , entityUserName = "user_username"
    , entityUserPassword = "user_password"
    }
  }

mapUserEntityToUser :: UserEntity R.Result -> User 
mapUserEntityToUser entity = 
  let 
    name = (entityUserName entity)  
    HashedPassword pass = entityUserPassword entity
  in createUser name pass