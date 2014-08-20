{-# LANGUAGE OverloadedStrings #-}
module Html (generate) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Maybe (fromMaybe)

import Text.Blaze (toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Cheapskate (markdown, def)
import Cheapskate.Html

import Highlight
import Types

generate :: Maybe String -> String -> [Chunk] -> T.Text
generate maybeCss name chunks = 
    let 
        lang = getLang name
        body = H.preEscapedToHtml $ map (chunkToHtml lang) chunks
        doc = preface maybeCss name body
    in 
        TL.toStrict $ renderHtml doc

(<++>) :: T.Text -> T.Text -> T.Text
(<++>) = T.append

preface :: Maybe String -> String -> H.Html -> H.Html
preface maybeCss fileName bodyHtml =
    let 
        cssPath = fromMaybe "" maybeCss
        cssAttr = toValue cssPath
        includeCss = 
            if cssPath /= ""
            then H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href cssAttr
            else H.toHtml T.empty
    in 
        H.docTypeHtml $ do 
        H.head $ do
            H.title $ H.toHtml fileName
            H.meta ! A.charset "UTF-8" 
            includeCss
        H.body $ do bodyHtml
            

chunkToHtml :: String -> Chunk -> H.Html
chunkToHtml lang chunk =
    case chunk of
    Prose txt -> H.toHtml $ markdown def txt
    Def _ name parts -> 
        let 
            header = headerToHtml name
            htmlParts = H.preEscapedToHtml $ map (partToHtml lang) parts
        in 
            H.pre $ H.code $ (header >> htmlParts)

headerToHtml :: T.Text -> H.Html
headerToHtml name =  H.preEscapedToHtml $ "&lt;&lt; " <++> link <++> " &gt;&gt;=\n" 
    where
        link = "<a id=\"" <++> underscored <++> "\" href=\"#" <++> underscored <++> "\">" <++> slim <++> "</a>"
        slim = T.strip name
        underscored = underscore slim

partToHtml :: String -> Part -> H.Html
partToHtml lang part =
    case part of
    Code txt -> highlight lang txt
    Ref txt -> H.preEscapedToHtml  ("&lt;&lt; " <++> link <++> " &gt;&gt;\n")
        where
            link = "<a href=\"#" <++> underscored <++> "\">" <++> slim <++> "</a>"
            slim = T.strip txt
            underscored = underscore slim 

underscore :: T.Text -> T.Text
underscore txt =
    T.pack $ concatMap (\c -> if c == ' ' then "_" else [c]) $ T.unpack txt
