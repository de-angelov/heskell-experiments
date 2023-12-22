{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE InstanceSigs #-}


module Types (
  AppConfig(..), 
  HasServantPort,
  HasDbPool,
  User,
  UserLoginData(..),
  UserPassword, 
  dbPoolL,
  servantPortL,
  createUser, 
  createUserPassword
  ) where 
import RIO
import Data.Aeson (FromJSON, ToJSON)
import qualified Servant as S
import Control.Monad.Error.Class (MonadError)
import Text.Blaze.XHtml5 (ToMarkup)
import Servant.Auth.JWT (FromJWT)
import Servant.Auth.Server (ToJWT)
import Hasql.Pool (Pool)

class HasDbPool env where 
  dbPoolL :: Lens' env Pool

class HasServantPort env where 
  servantPortL :: Lens' env Int 

data AppConfig
  = AppConfig
  { appLogFunc :: !LogFunc
  , appPort :: !Int
  , appDBPool :: !Pool
  }

instance HasDbPool AppConfig where
  dbPoolL = lens appDBPool (\x y -> x { appDBPool = y }) 

instance MonadError S.ServerError (RIO a) where
  throwError = throwIO 

instance HasLogFunc AppConfig where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasServantPort AppConfig where 
  servantPortL = lens appPort (\x y -> x { appPort = y })

newtype UserPassword 
  = UserPassword Text 
  deriving (Show, Eq, Generic,  FromJSON, ToJSON, ToJWT, ToMarkup)

data UserLoginData
  = UserLoginData 
  { username :: !Text 
  , password :: !UserPassword
  } deriving (Show, Eq, Generic)

data User 
  = User 
  { username :: !Text
  , id :: !Text
  } deriving (Show, Eq,  Generic, FromJSON, ToJSON, FromJWT, ToJWT, ToMarkup)

createUser :: Text -> Text -> User 
createUser = undefined

createUserPassword :: Text -> UserPassword
createUserPassword = undefined