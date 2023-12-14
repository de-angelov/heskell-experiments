{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric  #-}


module Types (
  AppConfig(..), 
  HasServantPort,
  User,
  UserPassword, 
  createUser, 
  createUserPassword
  ) where 
import RIO
import Data.Aeson (FromJSON, ToJSON)

class HasServantPort env where 
  servantPortL :: Lens' env Int 

data AppConfig
  = AppConfig
  { appLogFunc :: !LogFunc
  , appPort :: !Int
  }

instance HasLogFunc AppConfig where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasServantPort AppConfig where 
  servantPortL = lens appPort (\x y -> x { appPort = y })

newtype UserPassword 
  = UserPassword Text 
  deriving (Show, Eq, Generic,  FromJSON, ToJSON)

data User 
  = User 
  { username :: Text
  , password :: UserPassword 
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

createUser :: Text -> Text -> Text -> User 
createUser = undefined

createUserPassword :: Text -> UserPassword
createUserPassword = undefined