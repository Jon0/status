module Route where

import System.IO
import Network.Socket
import Network.Stream
import Network.HTTP
import File

element :: Int -> [a] -> Maybe a
element index array
    | index < length array = Just $ array !! index
    | otherwise = Nothing


response_header :: String -> String
response_header content = ("HTTP/1.1 200 OK\nContent-Length: " ++ (show (length content)) ++ "\n\n")


get_location :: String -> IO String
get_location f = do
    content <- getMainPage
    return (response_header content ++ content)


replyFn :: Handle -> IO ()
replyFn hdl = do
    inpStr <- hGetLine hdl
    print $ words inpStr
    case element 1 (words inpStr) of
        Nothing -> do
            hPutStrLn hdl "HTTP/1.1 404 Not Found";
        Just a -> do
            response <- get_location a
            hPutStrLn hdl response

--
data Test = Sometype Int | B Bool

test_fn :: Test
test_fn = Sometype 0


req_handler :: Request String -> (String, Bool)
req_handler request = ("test", True)

req_handler2 :: Request a -> (String, Bool)
req_handler2 request = ("test", True)

read_request :: (Stream s) => s -> IO String
read_request s = do return "test"
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

read_requests :: HandleStream Socket -> IO String
read_requests sock = do
    -- http_data <- receiveHTTP sock
    return "test"
