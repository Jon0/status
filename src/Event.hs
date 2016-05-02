module Event where

class Input i where
    fetch :: IO i

class Output o where
    send :: o -> IO ()
