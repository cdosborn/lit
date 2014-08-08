{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import Prelude hiding (head)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Cheapskate (markdown, def)
import Cheapskate.Html
import Text.Highlighting.Kate.Format.HTML
import Text.Blaze
import Text.Blaze.Html5 hiding (map, mark)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Types

mark :: [Chunk] -> T.Text
mark chunks = TL.toStrict $ renderHtml $ preface $ preEscapedToHtml $ map chunkToHtml chunks

preface rest = docTypeHtml $ do 
    head $ do
--      title "Introduction page."
        link ! rel "stylesheet" ! type_ "text/css" ! href "default.css"
    body $ do rest

chunkToHtml :: Chunk -> Html
chunkToHtml chunk =
    case chunk of
    Prose txt -> toMarkup $ markdown def txt
    Def _ name parts -> 
        let 
            header = headerToHtml name
            htmlParts = preEscapedToHtml $ map partToHtml parts
        in pre $ code $ (header >> htmlParts)

partToHtml :: Part -> Html
partToHtml part =
    case part of
    Code txt -> toHtml txt
    Ref txt -> preEscapedToHtml  ("<< " `T.append` link `T.append` " >>\n")
        where
            link = "<a href=\"#" `T.append` slim `T.append` "\">" `T.append` slim `T.append` "</a>"
            slim = T.strip txt

headerToHtml :: T.Text -> Html
headerToHtml name =  
    preEscapedToHtml $ "<< " `T.append` link `T.append` " >>=\n"
    where
        link = "<a id=\"" `T.append` slim `T.append` "\" href=\"#" `T.append` slim `T.append` "\">" `T.append` slim `T.append` "</a>"
        slim = T.strip name
