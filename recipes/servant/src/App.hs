{-# LANGUAGE StandaloneDeriving #-}

module App where 

import RIO
import RIO.Text(unpack)
import Crypto.JOSE (JWK)
import qualified Servant as S
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Database.SQLite.Simple ( open, Connection ) 
import Database.SQLite.Simple.FromRow ( FromRow )



newtype AppEnv
  = AppEnv {dbFile :: FilePath}
  deriving (Eq, Show, Generic, FromRow )

instance MonadFail (RIO AppEnv) where
  fail :: String -> RIO AppEnv a
  fail = error


class HasDbFile env where 
  getDbConnection' :: env -> IO Connection 
  getDbName' :: env -> FilePath 

instance HasDbFile AppEnv where 
  getDbConnection' :: AppEnv -> IO Connection
  getDbConnection' env = open . fromString  $ env.dbFile 
  
  getDbName' :: AppEnv -> FilePath
  getDbName' env = env.dbFile  

-- class HasJwtKey env where 
--   getJwtKey :: env -> JWK 

-- instance HasJwtKey AppEnv where 
--   getJwtKey :: AppEnv -> JWK
--   getJwtKey = envJwtKey

type AppM = RIO AppEnv 


runHandler :: AppEnv -> AppM a -> S.Handler a
runHandler env app = S.Handler $ ExceptT $ try $ runRIO env app

getDbConnection :: AppM (IO Connection)
getDbConnection = ask <&> getDbConnection'

getDbName :: AppM FilePath
getDbName = ask <&> getDbName' 