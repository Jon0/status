module Http where

import Data.Char
import Data.List
import Network.HTTP
import Network.Stream
import System.IO
import Numeric
import Content
import File
import Util


-- http types
type HttpKey = String
type HttpValue = String

data HttpPair = HttpPair {
    httpKey :: HttpKey,
    httpVal :: HttpValue
}

-- implement serialisable
instance Show HttpPair where
    show (HttpPair k v) = (k ++ ": " ++ v)


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
    httpCode :: Integer,
    header :: [HttpPair],
    content :: StreamTransfer
}

data HttpResponseHandler = HttpResponseHandler {
    responseData :: HttpResponse,
    responseSource :: StreamSet
}

-- either a content producer, or a link to one
data RouteItem =
    RouteLeaf (HttpRequest -> IO HttpResponseHandler)
    | RouteNode (String -> Maybe RouteItem)



-- types which can appear as routes
class RouteType r where
    routeName :: r -> String
    routeKey :: r -> [String]
    routeMap :: r -> RouteItem


showRequest :: HttpRequest -> String
showRequest request = ((show (urlSplit request)) ++ "\n" ++ (query request) ++ "\n")


-- matches path to content producer
routeMatch :: RouteItem -> [String] -> Maybe (HttpRequest -> IO HttpResponseHandler)
routeMatch (RouteLeaf c) _ = Just $ c
routeMatch (RouteNode n) [] = case (n "") of
    Nothing -> Nothing
    Just r -> routeMatch r []
routeMatch (RouteNode n) path = case (n (head path)) of
    Nothing -> Nothing
    Just r -> routeMatch r (tail path)


-- similiar to route match
requestMatch :: RouteItem -> HttpRequest -> Maybe (IO HttpResponseHandler)
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


urlChar :: String -> String
urlChar ('%':xs) = [(hexDigitToChar xs)]
urlChar _ = ""


-- replace symbols in url
urlReplace :: String -> String
urlReplace "" = ""
urlReplace url = let (a, b) = break (=='%') url in
    a ++ (urlChar (take 3 b)) ++ (urlReplace (drop 3 b))


urlSplitString :: String -> [String]
urlSplitString str = filterEmpty (wordDelim (=='/') str)


urlSplit :: HttpRequest -> [String]
urlSplit req = urlSplitString (urlReplace (urlString req))


subUrl :: Int -> HttpRequest -> String
subUrl index request = intercalate "/" $ drop 1 (urlSplit request)

-- split path into 2 parts
breakRequest :: HttpRequest -> Int -> (String, String)
breakRequest req index = ((absolutePath (take index str)), (absolutePath (drop index str))) where
    str = (urlSplit req)


codeName :: Integer -> String
codeName code =
    case code of
        200 -> "OK"
        400 -> "Bad Request"
        404 -> "Not Found"
        otherwise -> "Unknown"

-- create the first header response line
makeResponseLine :: Integer -> String
makeResponseLine code = ("HTTP/1.1 " ++ (show code) ++ " " ++ (codeName code))




-- creates a general response from a string of content
generalResponse :: String -> HttpResponse
generalResponse content = streamResponse (createStringTransfer content)


maybeHeaderElement :: (Show t) => String -> Maybe t -> Maybe HttpPair
maybeHeaderElement label mb =
    case mb of
        Nothing -> Nothing
        Just obj -> Just $ HttpPair label (show obj)


streamResponseLength :: StreamTransfer -> Maybe HttpPair
streamResponseLength content =
    maybeHeaderElement "Content-Length" (transferLength content)


streamResponseType :: StreamTransfer -> Maybe HttpPair
streamResponseType content =
    maybeHeaderElement "Content-Type" (transferMimeType content)


streamResponse :: StreamTransfer -> HttpResponse
streamResponse c = (HttpResponse 200 h c) where
    h = mapFnMaybe [streamResponseLength, streamResponseType] c


headerFromPairs :: [(StreamTransfer -> HttpPair)] -> StreamTransfer -> [HttpPair]
headerFromPairs fns str = mapFn fns str


responseHeadString :: HttpResponse -> String
responseHeadString r = ((makeResponseLine (httpCode r)) ++ (intercalate "\n" (map show (header r))) ++ "\n\n")


sendAllResponse :: Handle -> HttpResponse -> IO ()
sendAllResponse hdl response = do
    hPutStr hdl (responseHeadString response)
    sendAllContent hdl (content response) (16 * 1024)


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
