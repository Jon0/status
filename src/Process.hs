module Process where

import Data.Hashable
import File
import Config
import Content
import Document
import Html
import Http
import Util


data ProcessPage = ProcessPage {
    attrs :: [String],
    pids :: [Integer]
}


instance RouteType ProcessPage where
    routeName main = "/pid"
    routeKey main = []
    routeMap main = RouteNode (processPageMap main)


instance Renderable ProcessPage where
    renderAll p = [(renderRow p)]
    renderRow p = simpleRow ("attrs" : (attrs p))
    staticUrl p = Nothing


createProcessPage :: Config -> IO ProcessPage
createProcessPage cfg = do
    attrs <- showDirectory "/proc/self"
    return $ ProcessPage attrs []


processPageMap :: ProcessPage -> String -> Maybe RouteItem
processPageMap p s =
    case findElement (== s) (attrs p) of
        Nothing -> Nothing
        Just e -> Just $ RouteLeaf (processPageHandler p)


processPageHandler :: ProcessPage -> HttpRequest -> IO HttpResponseHandler
processPageHandler p request = do
    return $ HttpResponseHandler (generalResponse (toHtml (renderRow p))) emptyStreamSet


data TaskModification = FilePath | Handle

data TaskInput = TaskInput {
    taskArgs :: [String]
}


data TaskOutput = TaskOutput {
    resultModification :: [TaskModification]
}


data TaskObservation = TaskObservation {
    obs :: String
}


-- instance of hashable?
class (Hashable t) => Task t where
    indentifier :: t -> String
    run :: t -> TaskInput -> TaskOutput
    observe :: t -> TaskObservation


daemonPid :: IO (Maybe Int)
daemonPid = do
    return Nothing


createTaskFile :: (Task t) => FilePath -> t -> IO ()
createTaskFile file task = do
    return ()


addTask :: (Task t) => t -> IO ()
addTask task = do
    pid <- daemonPid
    case pid of
        Nothing -> do
            return ()
        Just p -> do
            createTaskFile (queuePath ++ (show $ hash task)) task



lockFile :: FilePath -> IO ()
lockFile s = do
    return ()
