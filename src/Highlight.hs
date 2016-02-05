{-# LINE 13 "src/Highlight.hs.lit" #-}
{-# LINE 23 "src/Highlight.hs.lit" #-}
module Highlight (highlight, getLang) where
{-# LINE 27 "src/Highlight.hs.lit" #-}
import qualified Data.Text as T 
import Data.Monoid (mconcat)

import Text.Blaze (toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Highlighting.Kate ( defaultFormatOpts
                              , highlightAs
                              , languagesByFilename )
import Text.Highlighting.Kate.Types 
{-# LINE 40 "src/Highlight.hs.lit" #-}
highlight :: String -> T.Text -> H.Html
highlight lang txt = 
    let
        highlighted = highlightAs lang (T.unpack txt)
        htmlList = map sourceLineToHtml highlighted
    in 
        mconcat htmlList
{-# LINE 50 "src/Highlight.hs.lit" #-}
sourceLineToHtml :: SourceLine -> H.Html
sourceLineToHtml line = mconcat $  htmlList ++ [H.toHtml "\n"]
    where
        htmlList = map (tokenToHtml defaultFormatOpts) line
{-# LINE 57 "src/Highlight.hs.lit" #-}
tokenToHtml :: FormatOptions -> Token -> H.Html
tokenToHtml _ (NormalTok, str)  = H.toHtml str
tokenToHtml opts (toktype, str) =
    if titleAttributes opts
    then sp ! A.title (toValue $ show toktype)
    else sp 
        where sp = H.span ! A.class_ (toValue $ short toktype) $ H.toHtml str
{-# LINE 67 "src/Highlight.hs.lit" #-}
short :: TokenType -> String
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
{-# LINE 85 "src/Highlight.hs.lit" #-}
getLang path = 
    case languagesByFilename path of
    [] -> ""
    lst -> head lst
