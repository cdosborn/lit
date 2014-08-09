{-# LANGUAGE OverloadedStrings #-}
module Processing where
import Prelude hiding (head, readFile, writeFile) -- also id and div
import Data.Text.IO (writeFile, readFile)

import Data.Function (fix)
import Data.List (partition)
import Control.Monad (zipWithM)
import Data.Sequence (Seq, singleton)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

import Parse
import Pretty
import Types

buildAll codeDir htmlDir files =
    let htmlOutputPaths = map (\f -> htmlDir ++ (fileNameFromPath f) ++ ".html") files 
        codeOutputPaths = map (\f -> codeDir ++ (fileNameFromPath f)) files 
        --htmlPipeline = (\out enc -> putStrLn (show enc))
        htmlPipeline = (\out enc -> (writeFile out) $ Pretty.mark (getLang out) enc)
        codePipeline = (\out enc -> (writeFile out) . expand $ merge enc)
    in do
        streams <- mapM readFile files 
        encoded <- mapM (\str -> return $ encode str) streams 
        (zipWithM codePipeline codeOutputPaths encoded)
        (zipWithM htmlPipeline htmlOutputPaths encoded) >> return ()

buildHtml htmlDir files =
    let htmlOutputPaths = map (\f -> htmlDir ++ (fileNameFromPath f) ++ ".html") files 
        streams = mapM readFile files
        pipeline = (\out f -> (writeFile out) . (Pretty.mark (getLang out)) $ encode f)
    in (zipWithM pipeline htmlOutputPaths =<< streams) >> return ()

buildCode codeDir files =
    let codeOutputPaths = map (\f -> codeDir ++ (fileNameFromPath f)) files 
        streams = mapM readFile files
        pipeline = (\out f -> (writeFile out) . expand . merge $ encode f)
    in (zipWithM pipeline codeOutputPaths =<< streams) >> return ()



merge :: [Chunk] -> [Chunk]
merge chunks = mergeAux [] (filter isDef chunks)
mergeAux ans [] = ans
mergeAux ans (next:rest) = 
    let 
        name = getName next
        (found, rem) = partition (sameName name) rest 
        merged = combineChunks (next:found)
    in 
        mergeAux (merged:ans) rem

-- always one or more chunk
combineChunks (a:[]) = a
combineChunks l@(c:cs) = Def line name parts 
    where
        parts = concatMap getParts l
        name = getName c
        line = getLineNo c

isDef chunk =
    case chunk of
    Def _ _ _ -> True
    Prose _ -> False

getName chunk =
    case chunk of
    Def _ name _ -> name
    _ -> error "cannot retrieve name, not a chunk"

getParts chunk =
    case chunk of
    Def _ _ parts -> parts
    _ -> error "cannot retrieve parts, not a chunk"

getLineNo chunk =
    case chunk of
    Def line _ _ -> line
    _ -> error "cannot retrieve line number, not a chunk"

sameName name chunk = name == (getName chunk)

expand :: [Chunk] -> T.Text
expand chunks =
    let 
        -- map (name, parts)
        partMap = Map.fromList $ zip (map getName chunks) (map getParts chunks)
        rootParts = Map.lookupDefault [] " * " partMap 
    in
        expandParts rootParts partMap
        
expandParts :: [Part] -> Map.HashMap T.Text [Part] -> T.Text
expandParts parts partMap =
    let 
        toText = (\part -> 
            case part of
            Code txt -> txt
            Ref name -> expandParts refParts partMap
                where refParts = Map.lookupDefault [] name partMap)
    in 
        T.concat (map toText parts)
