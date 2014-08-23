# Processing.hs - the glue linking `lit`'s command line interface and file generation
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

    process pipes file =
        let 
            fileName = dropExtension $ takeFileName file
        in do
            stream <- readFile file
            encoded <- return $ encode stream 
            mapM_ (\f -> f fileName encoded) pipes >> return ()

    htmlPipeline dir css name enc = 
        let 
            path = (ensureTrailingSlash dir) ++ name ++ ".html"
            output = Html.generate css name enc
        in 
            writeFile path output

    mdPipeline dir css name enc = 
        let
            path = (ensureTrailingSlash dir) ++ name ++ ".md"
            output = Markdown.generate name enc
        in
            writeFile path output

    codePipeline dir css name enc = 
        let
            path = (ensureTrailingSlash dir) ++ name
            output = Code.generate enc
        in
            writeFile path output

    ensureTrailingSlash dir = 
        if last dir == '/'
        then dir
        else dir ++ "/"

