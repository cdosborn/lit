{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Cheapskate (markdown, def)
import Cheapskate.Html
import Text.Highlighting.Kate (defaultFormatOpts, highlightAs, languagesByFilename)
import Text.Highlighting.Kate.Types 
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)

import Types

pretty :: String -> Maybe String -> [Chunk] -> T.Text
pretty lang maybeCss chunks = 
    TL.toStrict $ renderHtml $ preface maybeCss $ H.preEscapedToHtml $ map (chunkToHtml lang) chunks

mark :: String -> [Chunk] -> T.Text
mark lang chunks = T.concat $ map (chunkToMarkdown lang) chunks

chunkToMarkdown lang chunk =
    case chunk of
    Prose text  -> text
    Def _ name parts -> 
        let 
            lang' = T.pack lang
            header = headerToText name
            mdParts = T.concat $ map (partToText lang) parts
        in 
            "```" `T.append` lang' `T.append` "\n" `T.append` header `T.append` mdParts `T.append` "\n```\n"


preface :: Maybe String -> H.Html -> H.Html
preface mCss rest = H.docTypeHtml $ do 
    let css = toValue $ fromMaybe "" mCss
    H.head $ do
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href css
    H.body $ do rest
            

chunkToHtml :: String -> Chunk -> H.Html
chunkToHtml lang chunk =
    case chunk of
    Prose txt -> toMarkup $ markdown def txt
    Def _ name parts -> 
        let 
            header = headerToHtml name
            htmlParts = H.preEscapedToHtml $ map (partToHtml lang) parts
        in H.pre $ H.code $ (header >> htmlParts)

partToHtml :: String -> Part -> H.Html
partToHtml lang part =
    case part of
    Code txt -> mconcat $ map (sourceLineToHtml defaultFormatOpts) 
                        $ highlightAs lang (T.unpack txt)
    Ref txt -> H.preEscapedToHtml  ("<< " `T.append` link `T.append` " >>\n")
        where
            link = "<a href=\"#" `T.append` slim `T.append` "\">" `T.append` slim `T.append` "</a>"
            slim = T.strip txt

partToText :: String -> Part -> T.Text
partToText lang part =
    case part of
    Code txt -> txt
    Ref txt -> ("<< " `T.append` link `T.append` " >>\n")
        where
            link = "<a href=\"#" `T.append` slim `T.append` "\">" `T.append` slim `T.append` "</a>"
            slim = T.strip txt

headerToHtml :: T.Text -> H.Html
headerToHtml name =  H.preEscapedToHtml $ headerToText name

headerToText :: T.Text -> T.Text
headerToText name =  "<< " `T.append` link `T.append` " >>=\n"
    where
        link = "<a id=\"" `T.append` slim `T.append` "\" href=\"#" `T.append` slim `T.append` "\">" `T.append` slim `T.append` "</a>"
        slim = T.strip name

sourceLineToHtml :: FormatOptions -> SourceLine -> H.Html
sourceLineToHtml opts line = mconcat $ (map (tokenToHtml opts) line) ++ [(H.toHtml ("\n" :: String))]

tokenToHtml :: FormatOptions -> Token -> H.Html
tokenToHtml _ (NormalTok, txt)  = H.toHtml txt
tokenToHtml opts (toktype, txt) =
  if titleAttributes opts
     then sp ! A.title (toValue $ show toktype)
     else sp
   where sp = H.span ! A.class_ (toValue $ short toktype) $ H.toHtml txt

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
