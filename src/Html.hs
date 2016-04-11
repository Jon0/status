module Html where

import Data.List
import File


htmlPair :: (String, String) -> String
htmlPair (k, v) = k ++ "=\"" ++ v ++ "\""


htmlRef :: String -> String
htmlRef v = htmlPair ("href", v)


htmlTagOpenPairs :: String -> [(String, String)] -> String
htmlTagOpenPairs tag ps = htmlTagOpen tag (map htmlPair ps)


htmlTagOpen :: String -> [String] -> String
htmlTagOpen tag [] = ("<" ++ tag ++ ">")
htmlTagOpen tag opt = ("<" ++ tag ++ " " ++ opts ++ ">") where
    opts = (intercalate " " opt)


htmlTagClose :: String -> String
htmlTagClose tag = ("</" ++ tag ++ ">")


htmlTag :: String -> String -> String
htmlTag tag item = htmlTagOpt tag [] item


htmlTagOpt :: String -> [String] -> String -> String
htmlTagOpt tag opt item = ((htmlTagOpen tag opt) ++ item ++ (htmlTagClose tag))


htmlTagPairs :: String -> [(String, String)] -> String -> String
htmlTagPairs tag ps item = ((htmlTagOpenPairs tag ps) ++ item ++ (htmlTagClose tag))


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

-- combines a series of content types
joinContent :: [HtmlContent] -> String
joinContent es = concat $ map toHtml es


data HtmlDocument = HtmlDocument {
    header :: HtmlHeader,
    body :: HtmlBody
}


instance HtmlElement HtmlDocument where
    toHtml (HtmlDocument header body) = "<!DOCTYPE html>" ++ htmlTag "html" (toHtml header ++ toHtml body)


-- header type
data HtmlHeader = HtmlHeader {
    headerTitle :: String,
    headerIncludes :: [String]
}

instance HtmlElement HtmlHeader where
    toHtml h = htmlTag "head" ((htmlTag "title" (headerTitle h)) ++ (cssInclude (headerIncludes h)))

cssLink :: String -> String
cssLink s = htmlTagOpenPairs "link" [("rel", "stylesheet"), ("type", "text/css"), ("href", s)]


cssInclude :: [String] -> String
cssInclude i = concat $ map cssLink i



-- body type
data HtmlBody = HtmlBody { htmlContent :: [HtmlContent] }

instance HtmlElement HtmlBody where
    toHtml (HtmlBody c) = htmlTag "body" (joinContent c)


-- string type
data HtmlString = HtmlString String

instance HtmlElement HtmlString where
    toHtml (HtmlString s) = s

createLabel :: String -> HtmlContent
createLabel s = HtmlContent (HtmlString s)



-- heading type
data Heading = Heading Int String

instance HtmlElement Heading where
    toHtml (Heading s h) = let st = show s in
        htmlTag ("h" ++ st) h

createHtmlHeading :: Int -> String -> HtmlContent
createHtmlHeading size str = HtmlContent (Heading size str)


-- table type
data HtmlTable = HtmlTable { tableContent :: [[HtmlContent]] }

instance HtmlElement HtmlTable where
    toHtml t = toHtmlTable (tableContent t)


createHtmlTable :: [[HtmlContent]] -> HtmlContent
createHtmlTable ct = HtmlContent (HtmlTable ct)


toHtmlTable :: [[HtmlContent]] -> String
toHtmlTable tb = htmlTag "table" (concat $ map toHtmlTableRow tb)


toHtmlTableRow :: [HtmlContent] -> String
toHtmlTableRow line
    | length line > 0 = htmlTagPairs "tr" [("class", "tablerow")] (concat $ map toHtmlTableItem line)
    | otherwise = ""


toHtmlTableItem :: HtmlContent -> String
toHtmlTableItem item = htmlTag "td" (toHtml item)


-- form type
data HtmlForm = HtmlForm {
    formOpts :: [(String, String)],
    formElements :: [HtmlContent]
}

instance HtmlElement HtmlForm where
    toHtml form = htmlTagOpt "form" (map htmlPair (formOpts form)) (joinContent (formElements form))


createForm :: [(String, String)] -> [HtmlContent] -> HtmlContent
createForm o e = HtmlContent (HtmlForm o e)


-- input elements
data HtmlInput = HtmlInput {
    inputOpts :: [(String, String)]
}

instance HtmlElement HtmlInput where
    toHtml input = htmlTagOpen "input" $ map htmlPair (inputOpts input)

createInput :: [(String, String)] -> HtmlContent
createInput es = HtmlContent (HtmlInput es)


-- button elements
data HtmlButton = HtmlButton {
    buttonLabel :: String,
    buttonOpts :: [(String, String)]
}

instance HtmlElement HtmlButton where
    toHtml bt = htmlTagOpt "button" (map htmlPair (buttonOpts bt)) (buttonLabel bt)

createButton :: String -> [(String, String)] -> HtmlContent
createButton lb es = HtmlContent (HtmlButton lb es)


-- break elements
data HtmlBreak = HtmlBreak

instance HtmlElement HtmlBreak where
    toHtml b = htmlTagOpen "br" []

createBreak :: HtmlContent
createBreak = HtmlContent HtmlBreak


-- div elements
data HtmlDiv = HtmlDiv {
    divClass :: String,
    divElements :: [HtmlContent]
}

instance HtmlElement HtmlDiv where
    toHtml d = htmlTagOpt "div" [(htmlPair ("class", (divClass d)))] (joinContent (divElements d))

createDiv :: String -> [HtmlContent] -> HtmlContent
createDiv c e = HtmlContent (HtmlDiv c e)


-- general functions
labelHtml :: String -> HtmlContent
labelHtml s = createLabel s


-- tables into html tables
lineToHtml :: [String] -> [HtmlContent]
lineToHtml str = map labelHtml str


linesToHtml :: [[String]] -> [[HtmlContent]]
linesToHtml strs = (map lineToHtml strs)


pageWithHostName :: [HtmlContent] -> IO HtmlDocument
pageWithHostName body = do
    name <- getHostname
    return $ HtmlDocument (HtmlHeader name ["/swc/style.css"]) (HtmlBody body)


staticImage :: String -> String -> HtmlContent
staticImage s z = createLabel $ htmlTagOpen "img" [(htmlPair ("src", ("/swc/" ++ s))), (htmlPair ("height", z)), (htmlPair ("width", z))]
