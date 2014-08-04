module Chunk
( write ) where

import Control.Monad
import Control.Applicative
import Text.Regex
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.List (intersperse)


--regx = mkRegex "([^.]+?)\\.lit"
--    let r = mkRegex "([^.]+?)\\.lit"
--        m = matchRegex r sourcePath
--        lang = case m of
--            Just first::more -> first
--            Nothing -> "undefined"
--

fileNameFromPath :: String -> String
fileNameFromPath path =
    let r = mkRegex "(\\w+\\.\\w+)\\.lit$"
        m = matchRegex r path 
    in case m of 
        Just (fst:rest) -> fst
        Nothing -> ""

--write :: String -> [String] -> IO () 
write outputDir files = 
    let outputPaths = map (\f -> outputDir ++ (fileNameFromPath f)) files 
        contents = map process files
    in do 
        contents <- sequence contents 
        putStrLn $ concat $ intersperse " " outputPaths
        zipWithM_ BS.writeFile outputPaths contents

process :: String -> IO (BS.ByteString)
process file = do
    contents <- BS.readFile file
    return (BS.unlines (clean ( BS.lines contents)))

filterCode :: BS.ByteString -> BS.ByteString
filterCode line = 
    if (BS.take 4 line) == (BS.pack "    ") -- pull this out...
    then BS.drop 4 line
    else BS.empty

clean lines =
    let ls = map filterCode lines
        empty = BS.empty
    in filter (\bs -> bs /= empty) ls 
