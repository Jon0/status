module Html where

class Element e where
    toHtml :: e -> String

-- allow generic types of element
data DocContents a

-- or
data DocContent = forall e. Element e => MkElement e

pack :: Element e => e -> DocContent
pack = MkElement


data HtmlDocument = HtmlDocument {
    header :: HtmlHeader,
    content :: [DocContent]
}

instance Element HtmlDocument where
    toHtml (HtmlDocument header objects) = ""


-- header type
data HtmlHeader = HtmlHeader String

instance Element HtmlHeader where
    toHtml (HtmlHeader s) = ("<head><title>" ++ s ++ "</title></head>")


-- heading type
data Heading = Hd Int String

instance Element Heading where
    toHtml (Hd s h) = let st = show s in
        ("<h" ++ st ++ ">" ++ h ++ "</h" ++ st ++ ">")



-- table type
data Table = Table { tableContent :: [[String]] }


-- form type
data Form = Form { formElements :: [[String]] }



showElem :: (Element e) => [e] -> String
showElem es = concat $ map toHtml es


createHead :: String -> String
createHead s = ("<head><title>" ++ s ++ "</title></head>")

createBody :: String -> String
createBody s = "<body>" ++ s ++ "</body>"

createPage :: String -> String -> String
createPage name content = "<!DOCTYPE html><html>" ++ createHead name ++ createBody content ++ "</html>"


createPage2 :: String -> String -> String
createPage2 name content = let e = [Hd 4 "Test", Hd 4 "123"] in
    "<!DOCTYPE html><html>" ++ createHead name ++ createBody (showElem e) ++ "</html>"


-- generate :: String -> [Element]
template :: String -> [DocContent]
template content = [pack (Hd 4 "Test"), pack (Hd 4 "123")]
