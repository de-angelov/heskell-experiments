{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module StringUtils where 
import RIO 
import RIO.Text (unpack)

class Print a where
  makeString :: a -> String

instance {-# OVERLAPPING #-} Print String where
  makeString s = s

instance {-# OVERLAPPING #-} Print Utf8Builder where
  makeString s = s & utf8BuilderToText & unpack

instance Show a => Print a where
  makeString x = show x