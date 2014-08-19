{-# LANGUAGE OverloadedStrings #-}
module Pretty 
( pretty
, mark ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Cheapskate (markdown, def)
import Cheapskate.Html
import Text.Highlighting.Kate (defaultFormatOpts, highlightAs, languagesByFilename)
import Text.Highlighting.Kate.Types 
import Text.Blaze (toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)

import Types
import Util

pretty :: Maybe String -> String -> [Chunk] -> T.Text
pretty maybeCss name chunks = 
    let 
        lang = getLang name
        body = H.preEscapedToHtml $ map (chunkToHtml lang) chunks
        doc = preface maybeCss name body
    in 
        TL.toStrict $ renderHtml doc

mark :: String -> [Chunk] -> T.Text
mark name chunks = 
    let 
        lang = getLang name
        toMarkDown = chunkToMarkdown lang
    in
        T.concat $ map toMarkDown chunks

chunkToMarkdown lang chunk =
    case chunk of
    Prose text  -> text
    Def _ name parts -> 
        let 
            lang' = T.pack lang
            header = headerName name
            mdParts = T.concat $ map (partToText lang) parts
        in 
            "```" <++> lang'   <++> 
            "\n"  <++> header  <++> 
            "\n"  <++> mdParts <++> "```\n"

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

partToHtml :: String -> Part -> H.Html
partToHtml lang part =
    case part of
    Code txt -> mconcat $ map (sourceLineToHtml defaultFormatOpts) 
                        $ highlightAs lang (T.unpack txt)
    Ref txt -> H.preEscapedToHtml  ("&lt;&lt; " <++> link <++> " &gt;&gt;\n")
        where
            link = "<a href=\"#" <++> escaped <++> "\">" <++> slim <++> "</a>"
            slim = T.strip txt
            escaped = escape slim 

partToText :: String -> Part -> T.Text
partToText lang part =
    case part of
    Code txt -> txt
    Ref txt -> ("<< " <++> (T.strip txt) <++> " >>\n")

headerToHtml :: T.Text -> H.Html
headerToHtml name =  H.preEscapedToHtml $ "&lt;&lt; " <++> link <++> " &gt;&gt;=\n" 
    where
        link = "<a id=\"" <++> escaped <++> "\" href=\"#" <++> escaped <++> "\">" <++> slim <++> "</a>"
        slim = T.strip name
        escaped = escape slim 

escape :: T.Text -> T.Text
escape txt =
    T.pack $ concat $ map (\c -> if c == ' ' then "%20" else [c]) $ T.unpack txt

headerName :: T.Text -> T.Text
headerName name = "<< " <++> (T.strip name) <++> " >>="

-- The methods below were heavily derived from John MacFarlane's highlighting-kate source
tokenToHtml :: FormatOptions -> Token -> H.Html
tokenToHtml _ (NormalTok, txt)  = H.toHtml txt
tokenToHtml opts (toktype, txt) =
  if titleAttributes opts
     then sp ! A.title (toValue $ show toktype)
     else sp
   where sp = H.span ! A.class_ (toValue $ short toktype) $ H.toHtml txt

sourceLineToHtml :: FormatOptions -> SourceLine -> H.Html
sourceLineToHtml opts line = mconcat $ (map (tokenToHtml opts) line) ++ [(H.toHtml ("\n" :: String))]

short :: TokenType -> T.Text
short KeywordTok        = "kw"
short DataTypeTok       = "dt"
short DecValTok         = "dv"
short BaseNTok          = "bn"
short FloatTok          = "fl"
short CharTok           = "ch"
short StringTok         = "st"
short CommentTok        = "co"
short OtherTok          = "ot"
short AlertTok          = "al"
short FunctionTok       = "fu"
short RegionMarkerTok   = "re"
short ErrorTok          = "er"
short NormalTok         = ""

getLang path = 
    case languagesByFilename path of
    [] -> ""
    lst -> head lst
