{-# LANGUAGE OverloadedStrings #-}
module Code ( generate, generateWithAnnotation ) where
import Data.List (partition, intersperse)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Text.Parsec.Pos

import Types
generate :: [Chunk] -> T.Text
generate = expand . merge . (filter isDef)
generateWithAnnotation :: String -> [Chunk] -> T.Text
generateWithAnnotation ext = expand . merge . (annotate ext) . (filter isDef)

annotate :: String -> [Chunk] -> [Chunk]
annotate langExt chunks = map annotateChunk chunks
    where 
        annotateChunk (Def sourcePos name parts) = 
            Def sourcePos name $ (Code $ annotation sourcePos):parts
        annotation sourcePos = T.pack $ annotateForLang langExt (sourceName sourcePos) (sourceLine sourcePos) ++ "\n"
        annotateForLang ".sh" filePath lineNo = "# " ++ filePath ++ ":" ++ (show lineNo)
        annotateForLang ".hs" filePath lineNo = "-- " ++ filePath ++ ":" ++ (show lineNo)
        annotateForLang _ filePath lineNo = "// " ++ filePath ++ ":" ++ (show lineNo)
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
    expandParts parts partMap T.empty
    where 
        -- map (name, parts)
        partMap = Map.fromList $ zip (map getName chunks) (map getParts chunks)
        backup = getParts $ last chunks
        parts = Map.lookupDefault backup "*" partMap 
expandParts :: [Part] -> Map.HashMap T.Text [Part] -> T.Text -> T.Text
expandParts parts partMap baseIndent =
    T.concat $ map toText parts
    where 
        toText part =
            case part of
            Code txt -> T.append baseIndent txt
            Ref name indent -> (expandParts refParts partMap (T.append baseIndent indent))
                where refParts = Map.lookupDefault [] (T.strip name) partMap
