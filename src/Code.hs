{-# LINE 15 "src/Code.hs.lit" #-}
{-# LINE 22 "src/Code.hs.lit" #-}
{-# LANGUAGE OverloadedStrings #-}
module Code ( generate ) where
{-# LINE 29 "src/Code.hs.lit" #-}
import Data.List (partition, intersperse)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

import Types
{-# LINE 44 "src/Code.hs.lit" #-}
generate :: Bool -> String -> [Chunk] -> T.Text
generate numberLines ext = expand . merge . numberFn . (filter isDef)
    where
        numberFn = if numberLines then (addLineNumbers (getLineGenerator ext)) else id
{-# LINE 52 "src/Code.hs.lit" #-}
{-# LINE 60 "src/Code.hs.lit" #-}
getLineGenerator :: String -> (String -> Int -> T.Text)
getLineGenerator ".c" = \file line -> T.pack $ "#line " ++ (show line) ++ " \"" ++ file ++ "\"\n"
getLineGenerator ".hs" = \file line -> T.pack $ "{-# LINE " ++ (show line) ++ " \"" ++ file ++ "\" #-}\n"
getLineGenerator _ = \file line -> T.pack ""
{-# LINE 67 "src/Code.hs.lit" #-}
addLineNumbers :: (String -> Int -> T.Text) -> [Chunk] -> [Chunk]
addLineNumbers generator chunks = map numberDefs chunks
    where
        {-# LINE 76 "src/Code.hs.lit" #-}
        numberDefs (Def (SourceLoc srcFile srcLine) name parts) =
            Def (SourceLoc srcFile srcLine) name ((Code (generator srcFile (srcLine + 1))):(numberParts (generator srcFile) (srcLine + 1) parts))
        numberDefs chunk = chunk
        {-# LINE 85 "src/Code.hs.lit" #-}
        numberParts :: (Int -> T.Text) -> Int -> [Part] -> [Part]
        numberParts mkDirective cLine ((Ref name indent):(Code c):rest) =
            (Ref name indent):(Code (mkDirective (cLine + 1))):(Code c):(numberParts mkDirective (cLine + 2) rest)
        numberParts mkDirective cLine (part:rest) =
            part:(numberParts mkDirective (cLine + 1) rest)
        numberParts mkDirective cLine [] = []
{-# LINE 95 "src/Code.hs.lit" #-}
merge :: [Chunk] -> [Chunk]
merge = mergeAux []
{-# LINE 102 "src/Code.hs.lit" #-}
mergeAux ans [] = ans
mergeAux ans (next:rest) = 
    let 
        name = getName next
        chunkHasName name = (== name) . getName
        (found, rem) = partition (chunkHasName name) rest 
        merged = combineChunks (next:found)
    in 
        mergeAux (merged:ans) rem
{-# LINE 117 "src/Code.hs.lit" #-}
combineChunks :: [Chunk] -> Chunk
combineChunks (a:[]) = a
combineChunks l@(c:cs) = Def line name parts 
    where
        parts = concatMap getParts l
        name = getName c
        line = getLineNo c
{-# LINE 131 "src/Code.hs.lit" #-}
expand :: [Chunk] -> T.Text
expand chunks =
    let 
        -- map (name, parts)
        partMap = Map.fromList $ zip (map getName chunks) (map getParts chunks)
        backup = getParts $ last chunks
        parts = Map.lookupDefault backup "*" partMap 
    in
        expandParts parts partMap T.empty 
{-# LINE 147 "src/Code.hs.lit" #-}
expandParts :: [Part] -> Map.HashMap T.Text [Part] -> T.Text -> T.Text
expandParts parts partMap baseIndent =
    let 
        toText = (\part -> 
            case part of
            Code txt -> T.append baseIndent txt
            Ref name indent -> (expandParts refParts partMap (T.append baseIndent indent))
                where refParts = Map.lookupDefault [] (T.strip name) partMap)
    in 
        T.concat $ map toText parts
