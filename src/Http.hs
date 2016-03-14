module Http where

import Network.HTTP
import Network.Stream

-- http types
data HttpRequest = HttpRequest { path :: String, query :: String }

data HttpResponse = HttpResponse { header :: String, content :: String }

data RouteContent = RouteContent { respondFn :: (HttpRequest -> IO HttpResponse) }

data RouteItem =
    RouteLeaf { routeContent :: (HttpRequest -> IO HttpResponse) }
    | RouteNode { subRoutes :: (String -> Maybe RouteItem) }



-- types which can appear as routes
class RouteType r where
    routeName :: r -> String
    routeKey :: r -> [String]
    routeMap :: r -> RouteItem


-- matches path to content producer
routeMatch :: RouteItem -> [String] -> Maybe (HttpRequest -> IO HttpResponse)
routeMatch (RouteLeaf c) _ = Just $ c
routeMatch (RouteNode n) [] = Nothing
routeMatch (RouteNode n) path = case (n (head path)) of
    Nothing -> Nothing
    Just r -> routeMatch r (tail path)


-- unused
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
