module Document where


import Data.Maybe
import System.IO
import System.Directory
import File
import Html


-- filters out Nothing values
concatMaybe :: [Maybe a] -> [a]
concatMaybe (x:xs) = case x of
    Just a -> a : (concatMaybe xs)
    Nothing -> (concatMaybe xs)

-- convert file arrays of lines and words
splitLines :: String -> [[String]]
splitLines str = map words (lines str)


-- map function containing index of elements
mapIndexedInt :: (Int -> a -> b) -> Int -> [a] -> [b]
mapIndexedInt _ _ [] = []
mapIndexedInt fn index (ai:as) = (fn index ai) : (mapIndexedInt fn (index + 1) as)

mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed fn a = mapIndexedInt fn 0 a


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
