module Piece.Core.WithId (WithId (..)) where

import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.ToField (Action)
import Database.PostgreSQL.Simple.Types( Only (..), (:.) (..), )
import Data.Aeson (ToJSON)

import Piece.Core.Id (Id (..))

data WithId a = WithId 
      { withIdId :: Id a 
      , withIdVal :: a 
      } 
        deriving stock (Generic, Show, Eq)
        deriving anyclass (ToJSON)

instance FromRow a => FromRow (WithId a) where 
  fromRow :: RowParser (WithId a)
  fromRow = WithId <$> field <*> fromRow 
  {-# INLINE fromRow #-}

instance ToRow a => ToRow (WithId a) where 
  toRow :: WithId a -> [Action]
  toRow WithId{..} = toRow (Only withIdId :. withIdVal )
  {-# INLINE toRow #-}