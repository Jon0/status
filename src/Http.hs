module Http where

import Data.List
import Network.HTTP
import Network.Stream
import File
import Util


-- http types
data HttpVerb =
    HttpGet
    | HttpPost
    | HttpPut
    | HttpDelete

data HttpRequest = HttpRequest {
    reqVerb :: HttpVerb,
    urlString :: String,
    query :: String
}

data HttpResponse = HttpResponse {
    header :: [String],
    content :: String
}

-- either a content producer, or a link to one
data RouteItem =
    RouteLeaf (HttpRequest -> IO HttpResponse)
    | RouteNode (String -> Maybe RouteItem)



-- types which can appear as routes
class RouteType r where
    routeName :: r -> String
    routeKey :: r -> [String]
    routeMap :: r -> RouteItem


showRequest :: HttpRequest -> String
showRequest request = ((show (urlSplit request)) ++ "\n" ++ (query request) ++ "\n")


-- matches path to content producer
routeMatch :: RouteItem -> [String] -> Maybe (HttpRequest -> IO HttpResponse)
routeMatch (RouteLeaf c) _ = Just $ c
routeMatch (RouteNode n) [] = case (n "") of
    Nothing -> Nothing
    Just r -> routeMatch r []
routeMatch (RouteNode n) path = case (n (head path)) of
    Nothing -> Nothing
    Just r -> routeMatch r (tail path)


-- similiar to route match
requestMatch :: RouteItem -> HttpRequest -> Maybe (IO HttpResponse)
requestMatch routes req =
    case (routeMatch routes (urlSplit req)) of
        Nothing -> Nothing
        Just fn -> Just $ fn req


verbMatch :: String -> Maybe HttpVerb
verbMatch str
    | str == "GET" = Just HttpGet
    | str == "POST" = Just HttpPost
    | str == "PUT" = Just HttpPut
    | str == "DELETE" = Just HttpDelete
    | otherwise = Nothing


firstHeaderLine :: [String] -> Maybe HttpRequest
firstHeaderLine (verb:path:version:[]) =
    case (verbMatch verb) of
        Just httpverb -> let (file, query) = break (=='?') path in
            Just $ HttpRequest httpverb file query
        Nothing -> Nothing
firstHeaderLine _ = Nothing


urlSplitString :: String -> [String]
urlSplitString str = filterEmpty (wordDelim (=='/') str)


urlSplit :: HttpRequest -> [String]
urlSplit req = urlSplitString (urlString req)


-- split path into 2 parts
breakRequest :: HttpRequest -> Int -> (String, String)
breakRequest req index = ((absolutePath (take index str)), (absolutePath (drop index str))) where
    str = (urlSplit req)


codeName :: Int -> String
codeName code =
    case code of
        200 -> "OK"
        400 -> "Bad Request"
        404 -> "Not Found"
        otherwise -> "Unknown"

-- create the first header response line
makeResponseLine :: Int -> String
makeResponseLine code = ("HTTP/1.1 " ++ (show code) ++ " " ++ (codeName code))


-- creates a general response from a string of content
generalResponse :: String -> HttpResponse
generalResponse content = (HttpResponse header content) where
    header = [(makeResponseLine 200), ("Content-Length: " ++ (show (length content)))]


responseString :: HttpResponse -> String
responseString r = ((intercalate "\n" (header r)) ++ "\n\n" ++ (content r))


-- unused
type RequestHandler = Request_String -> IO Response_String

-- unused
req_handler :: Request String -> (String, Bool)
req_handler request = ("test", True)

-- unused
req_handler2 :: Request a -> (String, Bool)
req_handler2 request = ("test", True)

-- unused
readRequest :: HStream a => HandleStream a -> IO String
readRequest stream = do
    http_data <- receiveHTTP stream
    case http_data of
        Left e -> do
            return $ show e
        Right m -> do
            return $ show m
