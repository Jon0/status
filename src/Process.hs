module Process where

import Data.Hashable
import Config


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
