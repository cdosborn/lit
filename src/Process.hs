{-# LANGUAGE OverloadedStrings #-}
module Process
( process
, processPipe
, htmlPipeline
, mdPipeline
, codePipeline ) where

import Prelude hiding (readFile, writeFile, getContents, putStr)
import Data.Text.IO (writeFile, readFile, getContents, putStr)
import System.FilePath.Posix (takeFileName, dropExtension)
import System.Directory
import System.FilePath.Posix
import Data.List (intercalate)
import qualified Data.Text as T

import Parse (encode)
import Code
import Html
import Markdown
import Types

process pipes file = do
    stream <- readFile file
    processString writeFile stream fileName pipes >> return ()
    where
        fileName = dropExtension $ takeFileName file

processPipe pipes = do
    input <- getContents
    processString writeStdout input "-" pipes >> return ()
    where
        writeStdout = (\name output -> putStr output)

processString writeOutput input fileName pipes = do
    encoded <- return $ encode input
    mapM_ (\f -> f fileName encoded writeOutput) pipes >> return ()

htmlPipeline dir mCss mLang name enc writeOutput = do
    maybeCss <- cssRelativeToOutput dir mCss
    let path = (addTrailingPathSeparator dir) ++ name ++ ".html"
        output = Html.generate maybeCss mLang name enc
    writeOutput path output

mdPipeline dir css mLang name enc writeOutput = writeOutput path output
    where
        path = (addTrailingPathSeparator dir) ++ name ++ ".md"
        output = Markdown.generate mLang name enc

codePipeline dir css mLang name enc writeOutput = writeOutput path output
    where
        path = (addTrailingPathSeparator dir) ++ name
        output = Code.generate enc

cssRelativeToOutput :: String -> Maybe String -> IO (Maybe String)
cssRelativeToOutput output mCss =
    case mCss of
    Nothing -> return Nothing
    Just css -> do
    getCurrentDirectory >>= canonicalizePath >>= \path -> return $ Just $ (join' . helper' . trim' . split') path
    where 
        moves = filter (\str -> str /= ".") $ splitDirectories output
        split' = splitDirectories
        trim'   = trimToMatchLength moves
        helper' = reversePath moves []
        join' path = (intercalate "/" path) </> css

trimToMatchLength list listToTrim = 
    let len1 = length list
        len2 = length listToTrim
    in 
        drop (len2 - len1) listToTrim

reversePath [] solution curPathParts = solution
reversePath (fst:rest) solution curPathParts =
    if fst == ".."
    then reversePath rest ((last curPathParts) : solution) (init curPathParts)
    else reversePath rest (".." : solution) (curPathParts ++ [fst])



