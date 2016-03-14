module Http where

import Data.List
import Network.HTTP
import Network.Stream
import Util


-- http types
data HttpRequest = HttpRequest { urlString :: String, query :: String }

data HttpResponse = HttpResponse { header :: [String], content :: String }

data RouteContent = RouteContent { respondFn :: (HttpRequest -> IO HttpResponse) }

-- either a content producer, or a link to one
data RouteItem =
    RouteLeaf (HttpRequest -> IO HttpResponse)
    | RouteNode (String -> Maybe RouteItem)



-- types which can appear as routes
class RouteType r where
    routeName :: r -> String
    routeKey :: r -> [String]
    routeMap :: r -> RouteItem
    updateType :: [String] -> IO r


-- matches path to content producer
routeMatch :: RouteItem -> [String] -> Maybe (HttpRequest -> IO HttpResponse)
routeMatch (RouteLeaf c) _ = Just $ c
routeMatch (RouteNode n) [] = Nothing
routeMatch (RouteNode n) path = case (n (head path)) of
    Nothing -> Nothing
    Just r -> routeMatch r (tail path)

-- similiar to route match
requestMatch :: RouteItem -> HttpRequest -> Maybe (IO HttpResponse)
requestMatch routes req =
    case (routeMatch routes (urlSplit req)) of
        Nothing -> Nothing
        Just fn -> Just $ fn req


firstHeaderLine :: [String] -> Maybe HttpRequest
firstHeaderLine (verb:path:version:[]) =
    let (file, query) = break (=='?') path in
        Just $ HttpRequest file query
firstHeaderLine _ = Nothing


urlSplit :: HttpRequest -> [String]
urlSplit req = wordDelim (=='/') (urlString req)


codeName :: Int -> String
codeName code =
    case code of
        200 -> "OK"
        400 -> "Bad Request"
        404 -> "Not Found"
        otherwise -> "Unknown"


makeResponseLine :: Int -> String
makeResponseLine code = ("HTTP/1.1 " ++ (show code) ++ " " ++ (codeName code))


responseString :: HttpResponse -> String
responseString r = ((intercalate "\n" (header r)) ++ "\n" ++ (content r))


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
