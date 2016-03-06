module Config where

-- application state
data Config = Config { autoMount :: FilePath, hostPort :: Int }
