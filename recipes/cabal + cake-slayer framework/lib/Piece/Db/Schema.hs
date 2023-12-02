-- | Helper functions to create and drop database from @.sql@ files. 

module Piece.Db.Schema
  ( prepareDb 
  ) where 

import CakeSlayer.Db (WithDb, executeRaw_)

{- | Prepare database for the testing environmet: 

1. Drop all existing tables.
2. Created tables from scratch. 
-}
prepareDb :: (WithDb env m) => m ()
prepareDb = teardownDb >> setupDb >> seedDb 

-- | Create tables from the @sql/schema.sql@ file 
setupDb :: (WithDb  env m) => m ()
setupDb = executeFile "sql/schema.sql"

-- | Insert values from the @sql/seed.sql@ file/
seedDb :: (WithDb env m) => m ()
seedDb = executeFile "sql/seed.sql"

-- | Delete tables usign the @sql/drop/sql@ file. 
teardownDb :: (WithDb  env m) => m ()
teardownDb = executeFile "sql/drop.sql"


executeFile :: (WithDb env m) => FilePath -> m ()
executeFile path = do 
  sqlStatements <- readFile path 
  executeRaw_ (fromString sqlStatements)


