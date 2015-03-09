{-# LANGUAGE OverloadedStrings #-}
module Markdown ( generate ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Types
import Highlight (getLang)

generate :: Maybe String -> String -> [Chunk] -> T.Text
generate maybeLang name chunks = 
    let 
        lang = fromMaybe (getLang name) maybeLang
        toMarkDown = chunkToMarkdown lang
    in
        T.concat $ map toMarkDown chunks

(<++>) :: T.Text -> T.Text -> T.Text
(<++>) = T.append

chunkToMarkdown lang chunk =
    case chunk of
    Prose text  -> text
    Def _ name parts -> 
        let 
            lang' = T.pack lang
            header = "<< " <++> (T.strip name) <++> " >>="
            mdParts = T.concat $ map (partToText lang) parts
        in 
            "```" <++> lang'   <++> 
            "\n"  <++> header  <++> 
            "\n"  <++> mdParts <++> "```\n"

partToText :: String -> Part -> T.Text
partToText lang part =
    case part of
    Code txt -> txt
    Ref txt indent -> (indent <++> "<< " <++> (T.strip txt) <++> " >>\n")



