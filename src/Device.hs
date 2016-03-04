module Device where

import File

show_storage :: IO String
show_storage = do
    str <- showDirectory "/proc"
    return str
