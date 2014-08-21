{-# LANGUAGE OverloadedStrings #-}
module Code ( generate ) where

import Data.List (partition)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

import Types
  
generate :: [Chunk] -> T.Text
generate = expand . merge . (filter isDef)

-- merge together definitions with the same name
merge :: [Chunk] -> [Chunk]
merge = mergeAux [] 
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
