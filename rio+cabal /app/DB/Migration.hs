{-# LANGUAGE TemplateHaskell #-}

module DB.Migration where 

import RIO 
import qualified Hasql.Migration as M
import qualified Hasql.Migration.Util as Util
import qualified Hasql.Pool as P
import qualified Hasql.Transaction as T 
import qualified Data.FileEmbed as FE

scripts :: [(FilePath, ByteString)]
scripts = $(FE.embedDir "sql")

migrationCommands :: [M.MigrationCommand]
migrationCommands = map (uncurry M.MigrationScript) scripts

-- autoMigrate :: MonadIO m => P.Pool -> m (Maybe M.MigrationError)
-- autoMigrate pool = do 
--   runTransactionWithPool pool 