module Database where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

dbCreate :: FilePath -> IO ()
dbCreate p = do
    open p
