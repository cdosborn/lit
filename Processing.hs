module Processing where

import Data.Function (fix)
import Data.List (partition)
import Control.Monad (zipWithM)
import qualified Data.Map.Strict as Map

import Parse

buildAll htmlDir codeDir files =
    let codeOutputPaths = map (\f -> codeDir ++ (fileNameFromPath f)) files 
        htmlOutputPaths = map (\f -> htmlDir ++ (fileNameFromPath f)) files 
        streams = mapM readFile files
        pipeline = (\out f -> (writeFile out) . expand . merge $ encode f)
      --pipeline = (\out f -> (writeFile out) {-. expand . merge-}. show $ encode f)
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

expand :: [Chunk] -> String
expand chunks =
    let 
        -- map (name, parts)
        partMap = Map.fromList $ zip (map getName chunks) (map getParts chunks)
        rootParts = Map.findWithDefault [] " * " partMap 
    in
        expandParts rootParts partMap
        
expandParts :: [Part] -> Map.Map String [Part] -> String
expandParts parts partMap =
    let 
        toString = (\part -> 
            case part of
            Code str -> str
            Ref name -> expandParts refParts partMap
                where refParts = Map.findWithDefault [] name partMap)
    in 
        concat (map toString parts)
