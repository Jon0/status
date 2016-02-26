module Route where

import Network.Stream
import Network.HTTP


--
data Test = Sometype Int | B Bool

test_fn :: Test
test_fn = Sometype 0


req_handler :: Request String -> (String, Bool)
req_handler request = ("test", True)

read_request :: (Stream s) => s -> IO String
read_request str = do
    --input <- receiveHTTP str
    --case input of
    --    Nothing -> do
    --        return "error"
    --    Just request -> do
    --        (response, done) <- req_handler request
    --        return response
    return "."
