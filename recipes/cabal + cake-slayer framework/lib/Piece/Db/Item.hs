{-# LANGUAGE QuasiQuotes #-}
-- | SQL queries to work with the @items@ table.

module Piece.Db.Item 
    ( getItems
    , createItem
    , deleteItem 
    ) where 

import Relude.Extra.Bifunctor (firstF)
import Control.Monad.Except (liftEither)
import Database.PostgreSQL.Simple (fromOnly)
import CakeSlayer.Db (WithDb, queryRaw, returning, asSingleRowWith, executeNamed_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Piece.App.Error (WithError, AppError (..))
import PgNamed (PgNamedError, (=?))
import CakeSlayer.Error (ErrorWithSource)

import Piece.Core.Id (Id)
import Piece.Core.Item (Item)
import Piece.Core.WithId (WithId)


-- | Returns all items in the databse. 
getItems :: (WithDb env m) => m [WithId Item]
getItems = queryRaw [sql|
    SELECT id, text
    FROM items
|]

createItem :: (WithDb  env m, WithError m) => Item -> m (Id Item)
createItem item = 
  let 
    err :: AppError 
    err = DbError "expecting 'id' after inserting Item to 'items'"

    query = [sql|
            INSERT INTO items 
              (text)
            VALUES 
              (?)
            RETURNING id
          |] 

  in fmap fromOnly $ asSingleRowWith err $ returning query [item]


deleteItem :: (WithDb env m, WithError m) => Id Item -> m ()
deleteItem itemId = 
  let
    query = [sql| 
      DELETE FROM items
      WHERE id = ?id
    |]

    params =  [ "id" =? itemId ]
  in 
    fromPgNamedError $ executeNamed_ query params


fromPgNamedError :: WithError m => ExceptT (ErrorWithSource PgNamedError) m a -> m a 
fromPgNamedError action = 
  firstF (fmap DbNamedError) (runExceptT action) >>= liftEither 



  