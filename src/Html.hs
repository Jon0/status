module Html where

class Element e where
    toHtml :: e -> String

-- generate :: String -> [Element]
