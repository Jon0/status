module List where

import Document
import Html


renderListPage :: (Renderable r) => String -> [r] -> HtmlContent
renderListPage title rd = (renderList rd)

renderList :: (Renderable r) => [r] -> HtmlContent
renderList [] = labelHtml ""
renderList rd = HtmlContent $ HtmlTable (map renderAll rd)
