module Process where

import Data.Hashable
import Config


data TaskModification = FilePath | Handle


data TaskResult = TaskResult {
    resultModification :: [TaskModification]
}


data TaskObservation = TaskObservation {
    obs :: String
}


-- instace of hashable?
class (Hashable t) => Task t where
    indentifier :: t -> String
    run :: t -> TaskResult
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
