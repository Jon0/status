module Http where

import System.IO
import Network.HTTP
import Network.Socket


req_handler :: Request String -> (String, Bool)
req_handler request = ("test", True)

req_handler2 :: Request a -> (String, Bool)
req_handler2 request = ("test", True)

--read_request :: (Stream s) => s -> IO String
--read_request s = do return "test"
    --do
    --input <- receiveHTTP str
    --case input of
    --    Nothing -> do
    --        return "error"
    --    Just request -> do
    --        putStrLn $ receiveHTTP str
    --        (response, done) <- req_handler request
    --        return response
    --return "."
--read_request _ = do return "..."

--read_requests2 :: HandleStream Socket -> IO String
--read_requests2 sock = do
    -- http_data <- receiveHTTP sock
    --return "test"
