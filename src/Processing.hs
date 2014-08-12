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

--buildAll codeDir htmlDir files =
--    let htmlOutputPaths = map (\f -> htmlDir ++ (fileNameFromPath f) ++ ".html") files 
--        codeOutputPaths = map (\f -> codeDir ++ (fileNameFromPath f)) files 
--        htmlPipeline = (\out enc -> putStrLn (show enc))
--        --htmlPipeline = (\out enc -> (writeFile out) $ Pretty.mark (getLang out) $ simplify enc)
--        codePipeline = (\out enc -> (writeFile out) . expand $ merge enc)
--    in do
--        streams <- mapM readFile files 
--        encoded <- mapM (\str -> return $ encode str) streams 
--        (zipWithM codePipeline codeOutputPaths encoded)
--        (zipWithM htmlPipeline htmlOutputPaths encoded) >> return ()

buildAll mCss codeDir htmlDir file =
    let fileName = fileNameFromPath file
        htmlOutputPath = htmlDir ++ fileName ++ ".html"
        codeOutputPath = codeDir ++ fileName
        --htmlPipeline = (\out enc -> putStrLn (show enc))
        htmlPipeline = (\out enc -> (writeFile out) $ Pretty.mark (getLang out) mCss $ simplify enc)
        codePipeline = (\out enc -> (writeFile out) . expand $ merge enc)
    in do
        stream <- readFile file
        encoded <- return $ encode stream 
        codePipeline codeOutputPath encoded
        htmlPipeline htmlOutputPath encoded >> return ()

buildHtml mCss htmlDir file =
    let htmlOutputPath = htmlDir ++ (fileNameFromPath file) ++ ".html"
        stream = readFile file
        --pipeline = (\out f -> putStrLn (show $ simplify $ encode f))
        pipeline = (\out f -> (writeFile out) $ Pretty.mark (getLang out) mCss $ simplify $ encode f)
    in stream >>= pipeline htmlOutputPath >> return ()


--buildHtml htmlDir files =
--    let htmlOutputPaths = map (\f -> htmlDir ++ (fileNameFromPath f) ++ ".html") files 
--        streams = mapM readFile files
--        --pipeline = (\out f -> putStrLn (show $ simplify $ encode f))
--        pipeline = (\out f -> (writeFile out) $ Pretty.mark (getLang out) $ simplify $ encode f)
--    in (zipWithM pipeline htmlOutputPaths =<< streams) >> return ()

buildCode codeDir file =
    let codeOutputPath = codeDir ++ (fileNameFromPath file)
        stream = readFile file
        pipeline = (\out f -> (writeFile out) . expand . merge $ encode f)
    in stream >>= pipeline codeOutputPath >> return ()

--buildCode codeDir files =
--    let codeOutputPaths = map (\f -> codeDir ++ (fileNameFromPath f)) files 
--        streams = mapM readFile files
--        pipeline = (\out f -> (writeFile out) . expand . merge $ encode f)
--    in (zipWithM pipeline codeOutputPaths =<< streams) >> return ()

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
