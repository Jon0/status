module Document where

import System.IO
import System.Directory
import Html


data RenderMode = Basic | All
data DocStyle = DocStyle { renderMode :: RenderMode } --deriving (Show)

-- conversion to html
class DocNode d where
    createHtml :: d -> DocStyle -> IO HtmlContent


class Table t where
    tableType :: t -> [String]
