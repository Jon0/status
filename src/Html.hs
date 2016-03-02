module Html where

class Element e where
    toHtml :: e -> String


data Showable = forall a . Element a => MkElement a

pack :: Element a => a -> Showable
pack = MkElement

-- generate :: String -> [Element]


data Heading = Hd Int String

instance Element Heading where
    toHtml (Hd s h) = let st = show s in
        ("<h" ++ st ++ ">" ++ h ++ "</h" ++ st ++ ">")


showElem :: (Element e) => [e] -> String
showElem es = concat $ map toHtml es


createHead :: String -> String
createHead s = ("<head><title>" ++ s ++ "</title></head>")

createBody :: String -> String
createBody s = "<body>" ++ s ++ "</body>"

createPage :: String -> String -> String
createPage name content = let e = [Hd 4 "Test", Hd 4 "123"] in
    "<!DOCTYPE html><html>" ++ createHead name ++ createBody (showElem e) ++ "</html>"


template :: String -> [Showable]
template content = [pack (Hd 4 "Test"), pack (Hd 4 "123")]
