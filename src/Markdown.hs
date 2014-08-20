{-# LANGUAGE OverloadedStrings #-}
module Markdown ( generate ) where

import qualified Data.Text as T

import Text.Blaze (toValue, (!))

import Types
import Highlight (getLang)

generate :: String -> [Chunk] -> T.Text
generate name chunks = 
    let 
        lang = getLang name
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
            header = headerName name
            mdParts = T.concat $ map (partToText lang) parts
        in 
            "```" <++> lang'   <++> 
            "\n"  <++> header  <++> 
            "\n"  <++> mdParts <++> "```\n"

partToText :: String -> Part -> T.Text
partToText lang part =
    case part of
    Code txt -> txt
    Ref txt -> ("<< " <++> (T.strip txt) <++> " >>\n")

headerName :: T.Text -> T.Text
headerName name = "<< " <++> (T.strip name) <++> " >>="
