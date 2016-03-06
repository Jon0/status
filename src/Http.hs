module Http where

import Network.HTTP
import Network.Stream

type RequestHandler = Request_String -> IO Response_String


req_handler :: Request String -> (String, Bool)
req_handler request = ("test", True)


req_handler2 :: Request a -> (String, Bool)
req_handler2 request = ("test", True)


readRequest :: HStream a => HandleStream a -> IO String
readRequest stream = do
    http_data <- receiveHTTP stream
    case http_data of
        Left e -> do
            return $ show e
        Right m -> do
            return $ show m
