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

build mCss pipes file =
    let fileName = fileNameFromPath file
        lang = getLang fileName
    in do
        stream <- readFile file
        encoded <- return $ encode stream 
        mapM_ (\f -> f mCss lang fileName encoded) pipes >> return ()

htmlPipeline = (\dir css lang path enc -> writeFile ((ensureTrailingSlash dir) ++ path ++ ".html") $ pretty lang css $ simplify enc)
mdPipeline = (\dir css lang path enc -> writeFile ((ensureTrailingSlash dir) ++ path ++ ".md") $ (mark lang) enc)
codePipeline = (\dir css lang path enc -> writeFile ((ensureTrailingSlash dir) ++ path) $ expand $ merge enc)

ensureTrailingSlash dir = 
    if last dir == '/'
    then dir
    else dir ++ "/"

-- merge together definitions with the same name
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

-- many consecutive Proses are reduced to a single Prose
simplify :: [Chunk] -> [Chunk]
simplify [] = []
simplify lst =
    let (defs, ps) = span isDef lst
        (ps', rest) = break isDef ps
    in case ps' of
        [] -> defs ++ rest
        _ -> defs ++ [mergeProse ps'] ++ (simplify rest)

mergeProse :: [Chunk] -> Chunk
mergeProse lst = 
    Prose $ T.concat $ map getProseText lst

consecutive :: [Chunk] -> ([Chunk],[Chunk])
consecutive [] = ([],[])
consecutive lst@(fst:rest) =
    case fst of
    Prose _ -> break isDef lst
    Def _ _ _ -> span isDef lst

-- assumes always one or more chunk
combineChunks :: [Chunk] -> Chunk
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

getProseText prose =
    case prose of
    Prose txt -> txt
    _ -> error "cannot retrieve txt, not a prose"

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
