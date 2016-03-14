module Document where


import Data.Maybe
import System.IO
import System.Directory
import File
import Html
import Util


-- attributes for objects
class Linked l where
    getUrl :: l -> Maybe String

-- or use a renderable type containing all attributes
class Renderable r where
    render :: r -> IO HtmlContent
    staticUrl :: r -> Maybe String



data RenderMode = Basic | All
data DocStyle = DocStyle { renderMode :: RenderMode } --deriving (Show)


-- conversion to html
class DocNode d where
    createHtml :: d -> DocStyle -> IO HtmlContent


-- Table is a subtype of DocNode
class Table rowType where
    readLine :: [String] -> Maybe rowType
    showLine :: rowType -> [String]


data TableFile = TableFile { tablePath :: FilePath }


toTable :: (Table t) => [[String]] -> [t]
toTable tb = mapMaybe readLine tb


readTable :: (Table t) => FilePath -> IO [t]
readTable path = do
    prt <- fileContent path
    return $ toTable (splitLines prt)


showTable :: (Table t) => [t] -> [[String]]
showTable tb = (map showLine tb)


-- functions for appending rows
-- and transforming rows and columns


showTableMap :: (Table t) => (Int -> String -> String) -> [t] -> [[String]]
showTableMap _ [] = [[]]
showTableMap fn (ti:ts) = (mapIndexed fn (showLine ti)) : (showTableMap fn ts)
