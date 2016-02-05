{-# LINE 11 "src/Process.hs.lit" #-}
{-# LINE 19 "src/Process.hs.lit" #-}
{-# LANGUAGE OverloadedStrings #-}
module Process
( process
, htmlPipeline
, mdPipeline
, codePipeline ) where
{-# LINE 28 "src/Process.hs.lit" #-}
import Prelude hiding (readFile, writeFile)
import Data.Text.IO (writeFile, readFile)
import System.FilePath.Posix (takeFileName, dropExtension, combine, takeExtension)
import System.Directory
import System.FilePath.Posix
import Data.List (intercalate)
import qualified Data.Text as T

import Parse (encode)
import Code
import Html
import Markdown
import Types
{-# LINE 44 "src/Process.hs.lit" #-}
process pipes file = do 
    encoded <- encodeFile file
    mapM_ (\f -> f fileName encoded) pipes >> return ()
    where
        fileName = dropExtension $ takeFileName file

encodeFile :: String -> IO [Chunk]
encodeFile file = do
    stream <- readFile file
    expandInclude $ encode stream file
{-# LINE 59 "src/Process.hs.lit" #-}
expandInclude :: [Chunk] -> IO [Chunk]
expandInclude ((Include sourceName includedName):rest) = do
    tail <- expandInclude rest
    encodedInclude <- encodeFile includedFile
    return $ encodedInclude ++ tail
    where
        includedFile = combine (dropFileName sourceName) $ T.unpack includedName

expandInclude (chunk:rest) = do
    tail <- expandInclude rest
    return $ chunk:tail

expandInclude [] = return []
{-# LINE 80 "src/Process.hs.lit" #-}
{-# LINE 86 "src/Process.hs.lit" #-}
htmlPipeline dir mCss numberLines name enc = do
    maybeCss <- cssRelativeToOutput dir mCss
    let path = (addTrailingPathSeparator dir) ++ name ++ ".html"
        output = Html.generate maybeCss name enc
    writeFile path output
{-# LINE 93 "src/Process.hs.lit" #-}
mdPipeline dir css numberLines name enc = writeFile path output
    where
        path = (addTrailingPathSeparator dir) ++ name ++ ".md"
        output = Markdown.generate name enc
{-# LINE 99 "src/Process.hs.lit" #-}
codePipeline dir css numberLines name enc = writeFile path output
    where
        path = (addTrailingPathSeparator dir) ++ name
        output = Code.generate numberLines (takeExtension name) enc
{-# LINE 107 "src/Process.hs.lit" #-}
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
