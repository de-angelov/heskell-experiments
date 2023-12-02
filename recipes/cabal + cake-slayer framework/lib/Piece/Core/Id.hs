{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- | Polymorphic 'Id' type
-}
module Piece.Core.Id ( Id (..), AnyId, castId) where

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.Aeson (FromJSON, ToJSON)
import Elm (Elm (..))
import Elm.Generic (elmNewtype)
import Data.Type.Equality (type (==))

-- | Wrapper for integer id. Contains phantom type parameter for increased
-- type-safety.
newtype Id a = Id { unId :: Int }
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON)

instance Elm (Id a) where 
  toElmDefinition _ = elmNewtype @Int "Id" "unId"

-- | When we don't care about type of 'Id' but don't want to deal with type variables.
type AnyId = Id ()

-- | Unsafe cast of 'Id'. Implementation uses smart trick to enfore usage
-- always with @TypeApplications@. 
castId :: forall to from to' . ((to == to') ~ 'True) => Id from -> Id to' 
castId (Id a) = Id a 



