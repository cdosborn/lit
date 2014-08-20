{-# LANGUAGE OverloadedStrings #-}
module Process
( process
, htmlPipeline
, mdPipeline
, codePipeline ) where

import Prelude hiding (readFile, writeFile)
import Data.Text.IO (writeFile, readFile)
import System.FilePath.Posix (takeFileName, dropExtension)
import qualified Data.Text as T

import Parse
import Code
import Html
import Markdown
import Types

process pipes file = do 
    stream <- readFile file
    encoded <- return $ encode stream 
    mapM_ (\f -> f fileName encoded) pipes >> return ()
    where
        fileName = dropExtension $ takeFileName file

htmlPipeline dir css name enc = writeFile path output
    where 
        path = (ensureTrailingSlash dir) ++ name ++ ".html"
        output = Html.generate css name enc

mdPipeline dir css name enc = writeFile path output
    where
        path = (ensureTrailingSlash dir) ++ name ++ ".md"
        output = Markdown.generate name enc

codePipeline dir css name enc = writeFile path output
    where
        path = (ensureTrailingSlash dir) ++ name
        output = Code.generate enc

ensureTrailingSlash dir = 
    if last dir == '/'
    then dir
    else dir ++ "/"
