module Event where

class Input i where
    fetch :: IO i

class Output o where
    send :: o -> IO ()

class (Input d, Output d) => IODevice d where
    source :: d -> [Input d]
    dest :: d -> [Output d]

class Test t where
    apply :: a -> t a
