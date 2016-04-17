module Config where

import Text.Read
import Network.Socket

-- application state from command line args
data Config = Config {
    contentPath :: FilePath,
    hostPort :: PortNumber
}


defaultPath :: FilePath
defaultPath = "/srv"


defaultPort :: PortNumber
defaultPort = 8080


portString :: String -> Maybe PortNumber
portString s = fmap fromInteger (readMaybe s)


readOrDefault :: String -> PortNumber
readOrDefault s = case portString s of
    Just a -> a
    Nothing -> defaultPort


argsToConfig :: [String] -> Config
argsToConfig (dir:port:[]) = Config dir (readOrDefault port)
argsToConfig (dir:[]) = Config dir defaultPort
argsToConfig _ = Config defaultPath defaultPort


databasePath :: Config -> FilePath
databasePath cfg = ((contentPath cfg) ++ "/data")
