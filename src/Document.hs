module Document where


import Data.Maybe
import System.IO
import System.Directory
import Content
import File
import Html
import Util


-- attributes for objects
class Linked l where
    getUrl :: l -> Maybe String

-- or use a renderable type containing all attributes
class Renderable r where
    renderAll :: r -> [HtmlContent]
    renderRow :: r -> HtmlContent
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


tableToType :: (Table t) => ([String] -> [a]) -> [t] -> [[a]]
tableToType fn tb = map (fn . showLine) tb


toTable :: (Table t) => [[String]] -> [t]
toTable tb = mapMaybe readLine tb


readTableFile :: (Table t) => StreamSet -> FilePath -> IO (StreamSet, Maybe [t])
readTableFile set path = do
    (newSet, mHdl) <- contentOpen set path
    case mHdl of
        Nothing -> do
            return (newSet, Nothing)
        Just stream -> do
            items <- readTable stream
            return $ (newSet, Just items)


readTable :: (Table t) => DataStream -> IO [t]
readTable stream = do
    ct <- hGetContents (dataHandle stream)
    return $ toTable (splitLines ct)


showTable :: (Table t) => [t] -> [[String]]
showTable tb = (map showLine tb)


-- functions for appending rows
-- and transforming rows and columns


showTableMap :: (Table t) => (Int -> String -> String) -> [t] -> [[String]]
showTableMap _ [] = [[]]
showTableMap fn (ti:ts) = (mapIndexed fn (showLine ti)) : (showTableMap fn ts)
