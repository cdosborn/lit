module Types where

import Data.Text 

data Chunk = Def Int Text [Part] | Prose Text deriving (Show, Eq)
data Part = Code Text | Ref Text deriving (Show, Eq)
type Program = [Chunk]

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

