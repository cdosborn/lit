module Processing where

import Data.Function (fix)
import Data.List (partition)
import Control.Monad (zipWithM)
import Data.Sequence (Seq, singleton)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO (writeFile)


import Cheapskate
import Cheapskate.Html
import Cheapskate.Types
import Text.Blaze.Html.Renderer.Text
import Text.Blaze
import Parse

buildAll codeDir htmlDir files =
    let codeOutputPaths = map (\f -> codeDir ++ (fileNameFromPath f)) files 
        htmlOutputPaths = map (\f -> htmlDir ++ (fileNameFromPath f)) files 
        streams = mapM readFile files
        pipeline = (\out f -> (writeFile out) . expand . merge $ encode f)
      --pipeline = (\out f -> (writeFile out) {-. expand . merge-}. show $ encode f)
    in (zipWithM pipeline codeOutputPaths =<< streams) >> return ()

buildHTML htmlDir files =
    let htmlOutputPaths = map (\f -> htmlDir ++ (fileNameFromPath f)) files 
        streams = mapM readFile files
        pipeline = (\out f -> (Data.Text.IO.writeFile out) . mark $ encode f)
      --pipeline = (\out f -> (writeFile out) {-. expand . merge-}. show $ encode f)
    in (zipWithM pipeline htmlOutputPaths =<< streams) >> return ()

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
            Parse.Code str -> str
            Ref name -> expandParts refParts partMap
                where refParts = Map.findWithDefault [] name partMap)
    in 
        concat (map toString parts)

mark :: [Chunk] -> T.Text
mark chunks = T.concat $ map (TL.toStrict . renderHtml . chunkToHtml) chunks

chunkToHtml :: Chunk -> Markup
chunkToHtml chunk =
    case chunk of
    Prose str -> toMarkup $ markdown Cheapskate.def (T.pack str)
    Def _ _ parts -> 
        let 
            t = T.concat $ map partToText parts
            attr = CodeAttr { codeLang=(T.pack "haskell"), codeInfo=T.empty} :: CodeAttr
            block = CodeBlock attr t
            blocks = (singleton block) :: Seq Block
        in 
            renderBlocks Cheapskate.def blocks

type Apple = String
getApple :: Char -> Apple
getApple c = [c] :: String

partToText :: Part -> T.Text
partToText part =
    case part of
    Ref str -> T.pack ("<<" ++ str ++ ">>\n")
    Parse.Code str -> T.pack str
