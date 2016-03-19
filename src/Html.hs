module Html where

import Data.List
import File


htmlTag :: String -> String -> String
htmlTag tag item = htmlTagOpt tag [] item


htmlTagOpt :: String -> [String] -> String -> String
htmlTagOpt tag [] item = ("<" ++ tag ++ ">" ++ item ++ "</" ++ tag ++ ">")
htmlTagOpt tag opt item = ("<" ++ tag ++ " " ++ opts ++ ">" ++ item ++ "</" ++ tag ++ ">") where
    opts = (intercalate " " opt)


htmlPair :: String -> String -> String
htmlPair k v = k ++ "=\"" ++ v ++ "\""


htmlRef :: String -> String
htmlRef v = htmlPair "href" v

-- transformable elements
class HtmlElement e where
    toHtml :: e -> String
    -- dimension :: e -> [Int]
    -- subElements :: e -> [HtmlElement]


-- allow generic types of element
data HtmlContent = forall e. HtmlElement e => HtmlContent e
instance HtmlElement HtmlContent where
  toHtml (HtmlContent a) = toHtml a

-- example function
pack :: HtmlElement e => e -> HtmlContent
pack = HtmlContent


data HtmlDocument = HtmlDocument {
    header :: HtmlHeader,
    content :: [HtmlContent]
}

instance HtmlElement HtmlDocument where
    toHtml (HtmlDocument header objects) = ""


-- header type
data HtmlHeader = HtmlHeader String

instance HtmlElement HtmlHeader where
    toHtml (HtmlHeader s) = htmlTag "head" (htmlTag "title" s)



-- string type
data HtmlString = HtmlString String

instance HtmlElement HtmlString where
    toHtml (HtmlString s) = s

-- heading type
data Heading = Heading Int String

instance HtmlElement Heading where
    toHtml (Heading s h) = let st = show s in
        htmlTag ("h" ++ st) h

-- table type
data HtmlTable = HtmlTable { tableContent :: [[HtmlContent]] }

instance HtmlElement HtmlTable where
    toHtml t = toHtmlTable (tableContent t)


toHtmlTable :: [[HtmlContent]] -> String
toHtmlTable tb = htmlTag "table" (concat $ map toHtmlTableRow tb)


toHtmlTableRow :: [HtmlContent] -> String
toHtmlTableRow line
    | length line > 0 = htmlTag "tr" (concat $ map toHtmlTableItem line)
    | otherwise = ""


toHtmlTableItem :: HtmlContent -> String
toHtmlTableItem item = htmlTag "td" (toHtml item)


-- form type
data Form = Form { formElements :: [[String]] }


labelHtml :: String -> HtmlContent
labelHtml s = HtmlContent (HtmlString s)


showElem :: (HtmlElement e) => [e] -> String
showElem es = concat $ map toHtml es


createHead :: String -> String
createHead s = htmlTag "head" (htmlTag "title" s)


createBody :: String -> String
createBody s = htmlTag "body" s


createPage :: String -> String -> String
createPage name content = "<!DOCTYPE html>" ++ htmlTag "html" (createHead name ++ createBody content)


-- generate :: String -> [Element]
template :: String -> [HtmlContent]
template content = [pack (Heading 4 "Test"), pack (Heading 4 "123")]


-- tables into html tables
lineToHtml :: [String] -> [HtmlContent]
lineToHtml str = map labelHtml str


linesToHtml :: [[String]] -> [[HtmlContent]]
linesToHtml strs = (map lineToHtml strs)


pageWithHostName :: String -> IO String
pageWithHostName content = do
    name <- getHostname
    return $ createPage name content
