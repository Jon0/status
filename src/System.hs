module System where


data Update = Update { updateFn :: IO () }


class Resource r where
    resourceName ::r -> String
    resourceItems :: r -> [String]


class (Resource r) => ResourceSet r where
    updateItems :: IO [r]




updateTypes :: (ResourceSet r) => r -> IO ()
updateTypes set = do
    return ()
