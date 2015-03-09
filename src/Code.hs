{-# LANGUAGE OverloadedStrings #-}
module Code ( generate ) where

import Data.List (partition, intersperse)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

import Types

generate :: [Chunk] -> T.Text
generate = expand . merge . (filter isDef)

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
        partMap = Map.fromList $ zip (map getName chunks) (map (simplify. getParts) chunks)
        backup = getParts $ last chunks
        parts = Map.lookupDefault backup "*" partMap 
    in
        expandParts parts partMap T.empty

--expandParts :: [Part] -> Map.HashMap T.Text [Part] -> T.Text -> T.Text
--expandParts parts partMap baseIndent =
--    let 
--        toText = (\part -> 
--            case part of
--            Code txt -> T.append baseIndent txt
--            Ref name indent -> expandParts refParts partMap (T.append baseIndent indent)
--                where refParts = Map.lookupDefault [] (T.strip name) partMap)
--    in 
--        T.concat $ intersperse "\n" (map toText parts)

simplify :: [Part] -> [Part]
simplify [] = []
simplify parts =
    let 
        (codeParts, others) = break isRef parts
        (refParts, rest) = span isRef others
    in 
        refParts ++ (combineCodeParts codeParts) ++  (simplify rest)

combineCodeParts :: [Part] -> [Part]
combineCodeParts [] = []
combineCodeParts parts = [Code (T.concat (map getCodeText parts))]

expandParts :: [Part] -> Map.HashMap T.Text [Part] -> T.Text -> T.Text
expandParts parts partMap baseIndent =
    let 
        toText = (\part -> 
            case part of
            Code txt -> T.append baseIndent txt
            Ref name indent -> (expandParts refParts partMap (T.append baseIndent indent))
                where refParts = Map.lookupDefault [] (T.strip name) partMap)
    in 
        T.concat $ intersperse "\n" (map toText parts)
