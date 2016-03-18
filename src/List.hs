module List where

import Document
import Html


renderList :: (Renderable r) => [r] -> HtmlContent
renderList [] = labelHtml ""
renderList rd = HtmlContent $ HtmlTable (map renderAll rd)
