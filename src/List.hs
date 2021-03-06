module List where

import Data.List
import Document
import Html


renderListPage :: (Renderable r) => String -> [r] -> HtmlContent
renderListPage title rd = (renderList rd)


renderList :: (Renderable r) => [r] -> HtmlContent
renderList [] = labelHtml ""
renderList rd = HtmlContent $ HtmlTable (map renderAll rd)


renderListDiv :: (Renderable r) => [r] -> HtmlContent
renderListDiv rd = createDiv "list" $ [(renderList rd)]


renderZipDiv :: (Renderable (a, r)) => a -> [r] -> HtmlContent
renderZipDiv ad rd = renderListDiv (zip (cycle [ad]) rd)


renderSortedList :: (Renderable r) => (r -> r -> Ordering) -> [r] -> HtmlContent
renderSortedList order items = renderListDiv (sortBy order items)
