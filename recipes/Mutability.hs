{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Testing where

import Prelude
  ( IO
  , Integer
  , String
  , (<$>)
  , (<*>)
  , ($)
  , (+)
  , (==)
  , (>>=)
  , (>>)
  , getLine
  , pure
  , putStr
  , putStrLn  -- No need to import `System.IO.print`
  , show
  )

import Control.Monad.Reader (ReaderT, MonadReader(..), asks, runReaderT, unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar', readTVar)
import Control.Concurrent.STM (STM, atomically)
import qualified Data.List as List

--------------------------------------------------------------------------------
-- | App                                                                      --
--------------------------------------------------------------------------------
main :: IO ()
main = mkDefaultAppSettings >>= runApp

runApp :: AppSettings -> IO ()
runApp = runReaderT loopedAction
  where
  loopedAction :: ReaderT AppSettings IO ()
  loopedAction = do      

    incrementCount

    line <- liftIO do
      putStrLn "\nWrite something here"
      putStr "YOUR INPUT> " >>> getLine
    
    if line == "/stop"
      then
        liftIO $ putStrLn "\nProgram terminated!"
      else do
        counter <- getCount
        appSettings <- ask
        printOutput appSettings line counter 
        loopedAction

--------------------------------------------------------------------------------
-- | Stateful actions                                                         --
--------------------------------------------------------------------------------
withCounter
  :: (MonadReader AppSettings m, MonadIO m)
  => (TVar Integer -> STM a)
  -> m a
withCounter k = do
  tvar <- asks count
  liftIO $ atomically (k tvar)

getCount
  :: (MonadReader AppSettings m, MonadIO m)
  => m Integer
getCount = withCounter readTVar

incrementCount
  :: (MonadReader AppSettings m, MonadIO m)
  => m ()
incrementCount =
  withCounter \tvar ->
    modifyTVar' tvar (+1)

--------------------------------------------------------------------------------
-- | Settings                                                                 --
--------------------------------------------------------------------------------
data AppSettings = MkAppSettings
  { prefix  :: String
  , postfix :: String
  , count   :: TVar Integer
  }

mkDefaultAppSettings :: IO AppSettings
mkDefaultAppSettings =
  MkAppSettings <$> pure "<<<"
                <*> pure ">>>"
                <*> newTVarIO 0

-- 
-- -- Or...
-- mkDefaultAppSettings :: IO AppSettings
-- mkDefaultAppSettings =
--   liftA3
--     MkAppSettings
--     (pure "<<<")
--     (pure ">>>")
--     (newTVarIO 0)
--     
-- -- Or...
-- mkDefaultAppSettings :: IO AppSettings
-- mkDefaultAppSettings = do
--   count <- newTVarIO 0
--   pure $ MkAppSettings "<<<" ">>>" count

-- -- Or...
-- mkDefaultAppSettings :: IO AppSettings
-- mkDefaultAppSettings = do
--   count <- newTVarIO 0
--   pure $ MkAppSettings{..}
--   where
--     prefix = "<<<"
--     postfix = ">>>"

--------------------------------------------------------------------------------
-- | IO Utils                                                                 --
--------------------------------------------------------------------------------
printOutput :: MonadIO m => AppSettings -> String -> Integer -> m ()
printOutput (MkAppSettings{..}) line counter =
  liftIO $ putStrLn $
    List.intercalate " " [ prefix, line, "-- counter:", show counter, postfix ]
