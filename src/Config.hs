module Config where

-- application state
data Config = Config { contentPath :: FilePath, hostPort :: Int }
