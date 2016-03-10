module Document where

import System.IO
import System.Directory
import Html


data RenderMode = Basic | All
data DocStyle = DocStyle { renderMode :: RenderMode } --deriving (Show)

-- conversion to html
class DocNode d where
    createHtml :: d -> DocStyle -> IO HtmlContent


class Table rowType where
    readLine :: [String] -> Maybe rowType
    showLine :: rowType -> [String]



data TableFile = TableFile { tablePath :: FilePath }

readTable :: (Table t) => FilePath -> IO [t]
readTable path = do
    return []

--testTable :: (Table t) => [String] -> t
