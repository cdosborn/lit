{-# LANGUAGE OverloadedStrings #-}
module Processing 
( build
, htmlPipeline
, mdPipeline
, codePipeline ) where

import Prelude hiding (readFile, writeFile)
import Data.Text.IO (writeFile, readFile)
import System.FilePath.Posix (takeFileName, dropExtension)

import Data.List (partition)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

import Parse
import Pretty
import Types
import Util

build pipes file =
    let 
        fileName = dropExtension $ takeFileName file
    in do
        stream <- readFile file
        encoded <- return $ encode stream 
        mapM_ (\f -> f fileName encoded) pipes >> return ()

htmlPipeline = (\dir css name enc -> writeFile ((ensureTrailingSlash dir) ++ name ++ ".html") $ pretty css name enc)
mdPipeline = (\dir css name enc -> writeFile ((ensureTrailingSlash dir) ++ name ++ ".md") $ mark name enc)
codePipeline = (\dir css name enc -> writeFile ((ensureTrailingSlash dir) ++ name) $ T.strip $ expand $ merge enc)

-- merge together definitions with the same name
merge :: [Chunk] -> [Chunk]
merge chunks = mergeAux [] (filter isDef chunks)
mergeAux ans [] = ans
mergeAux ans (next:rest) = 
    let 
        name = getName next
        chunkHasName name = (== name) . getName
        (found, rem) = partition (chunkHasName name) rest 
        merged = combineChunks (next:found)
    in 
        mergeAux (merged:ans) rem

-- assumes always one or more chunk
combineChunks :: [Chunk] -> Chunk
combineChunks (a:[]) = a
combineChunks l@(c:cs) = Def line name parts 
    where
        parts = concatMap getParts l
        name = getName c
        line = getLineNo c

expand :: [Chunk] -> T.Text
expand chunks =
    let 
        -- map (name, parts)
        partMap = Map.fromList $ zip (map getName chunks) (map getParts chunks)
        rootParts = Map.lookupDefault [] "*" partMap 
    in
        expandParts rootParts partMap
        
expandParts :: [Part] -> Map.HashMap T.Text [Part] -> T.Text
expandParts parts partMap =
    let 
        toText = (\part -> 
            case part of
            Code txt -> txt
            Ref name -> expandParts refParts partMap
                where refParts = Map.lookupDefault [] (T.strip name) partMap)
    in 
        T.concat (map toText parts) `T.append` "\n"
