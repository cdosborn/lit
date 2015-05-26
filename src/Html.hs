{-# LINE 12 "src/Html.hs.lit" #-}
{-# LINE 19 "src/Html.hs.lit" #-}
{-# LANGUAGE OverloadedStrings #-}
module Html (generate) where
{-# LINE 26 "src/Html.hs.lit" #-}
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
{-# LINE 47 "src/Html.hs.lit" #-}
generate :: Maybe String -> String -> [Chunk] -> T.Text
generate maybeCss name chunks = 
    let 
        lang = getLang name
        mergedProse = simplify chunks -- adjacent Prose combined to one prose
        body = H.preEscapedToHtml $ map (chunkToHtml lang) mergedProse
        doc = preface maybeCss name body
    in 
        TL.toStrict $ renderHtml doc
{-# LINE 59 "src/Html.hs.lit" #-}
{-# LINE 70 "src/Html.hs.lit" #-}
(<++>) :: T.Text -> T.Text -> T.Text
(<++>) = T.append
{-# LINE 77 "src/Html.hs.lit" #-}
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
{-# LINE 101 "src/Html.hs.lit" #-}
simplify :: [Chunk] -> [Chunk]
simplify [] = []
simplify lst =
    let 
        (defs, ps) = span isDef lst
        (ps', rest) = break isDef ps
        mergeProse chunks = Prose $ T.concat $ map getProseText chunks
    in case ps' of
        [] -> defs ++ rest
        _ -> defs ++ [mergeProse ps'] ++ (simplify rest)
{-# LINE 117 "src/Html.hs.lit" #-}
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
{-# LINE 132 "src/Html.hs.lit" #-}
partToHtml :: String -> Part -> H.Html
partToHtml lang part =
    case part of
    Code txt -> highlight lang txt
    Ref txt indent -> H.preEscapedToHtml  (indent <++> "&lt;&lt; " <++> link <++> " &gt;&gt;\n")
        where
            link = "<a href=\"#" <++> underscored <++> "\">" <++> slim <++> "</a>"
            slim = T.strip txt
            underscored = underscore slim 
{-# LINE 146 "src/Html.hs.lit" #-}
headerToHtml :: T.Text -> H.Html
headerToHtml name =  H.preEscapedToHtml $ "&lt;&lt; " <++> link <++> " &gt;&gt;=\n" 
    where
        link = "<a id=\"" <++> underscored <++> "\" href=\"#" <++> underscored <++> "\">" <++> slim <++> "</a>"
        slim = T.strip name
        underscored = underscore slim
{-# LINE 157 "src/Html.hs.lit" #-}
underscore :: T.Text -> T.Text
underscore txt =
    T.pack $ concatMap (\c -> if c == ' ' then "_" else [c]) $ T.unpack txt
