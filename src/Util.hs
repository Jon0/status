module Util where

import Data.Char
import Numeric


map2D :: (a -> a) -> [[a]] -> [[a]]
map2D _ [] = []
map2D fn (x:xs) = (map fn x) : (map2D fn xs)


-- append an item if an equal item does not exist
appendUnique :: (Eq a) => [a] -> a -> [a]
appendUnique [] it = [it]
appendUnique arr@(a:as) it =
    if (a == it)
    then arr
    else a : appendUnique as it


-- filters out Nothing values
concatMaybe :: [Maybe a] -> [a]
concatMaybe (x:xs) = case x of
    Just a -> a : (concatMaybe xs)
    Nothing -> (concatMaybe xs)


-- add a prefix to each element
prefixSet :: [a] -> [[a]] -> [[a]]
prefixSet _ [] = []
prefixSet item (x:xs) = (item ++ x) : (prefixSet item xs)


elemOrEmpty :: Int -> [[a]] -> [a]
elemOrEmpty e s =
    if 0 <= e && e < length s
    then s !! e
    else []


-- remove empty items from a list
filterEmpty :: (Eq a) => [[a]] -> [[a]]
filterEmpty list = filter (/=[]) list


-- convert file arrays of lines and words
splitLines :: String -> [[String]]
splitLines str = map words (lines str)


-- split using delimiter removing empty strings
wordSplit :: (Char -> Bool) -> String -> [String]
wordSplit _ "" = []
wordSplit d s =
    let (a, b) = (break d s) in
        if (length b) == 0
        then []
        else a : (wordDelim d (tail b))


-- split using delimiter keeping empty strings
wordDelim :: (Char -> Bool) -> String -> [String]
wordDelim _ "" = []
wordDelim d s =
    let (a, b) = (break d s) in
        if (length b) == 0
        then [a]
        else a : (wordDelim d (tail b))


-- split while keeping empty strings
allWords :: String -> [String]
allWords s = wordDelim isSpace s


-- map function containing index of elements
mapIndexedInt :: (Int -> a -> b) -> Int -> [a] -> [b]
mapIndexedInt _ _ [] = []
mapIndexedInt fn index (ai:as) = (fn index ai) : (mapIndexedInt fn (index + 1) as)

mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed fn a = mapIndexedInt fn 0 a


-- safe element access
element :: Int -> [a] -> Maybe a
element index array
    | index < length array = Just $ array !! index
    | otherwise = Nothing


findElement :: (a -> Bool) -> [a] -> Maybe a
findElement _ [] = Nothing
findElement fn (x:xs) =
    if (fn x)
    then
        Just x
    else
        findElement fn xs


-- remove prefix x from y until non matching
stringRemove :: (Eq a) => [a] -> [a] -> [a]
stringRemove [] y = y
stringRemove x [] = []
stringRemove (x:xs) yr@(y:ys) =
    if x == y
    then stringRemove xs ys
    else yr


-- string of digits converted to char
hexDigitToChar :: String -> Char
hexDigitToChar xs =
    (chr c) where
        (c, n) = head (readHex xs)
