# [/ ](root.html)Types.hs - the data structure for a parse `lit` file

Process unifies the different ways that `lit` can produce files. Here are the goals:

1. Parse a file into a data structure
2. Pass the data structure through pipelines (Html, Code, Markdown)
3. Write the output to files

An overview of the file:
    << * >>=
    << define Process module >>
    << import modules >>
    << process a single file >>
    << helper functions >>

module Types where

import Data.Text 

data Chunk = Def Int Text [Part] | Prose Text deriving (Show, Eq)
data Part = Code Text | Ref Text deriving (Show, Eq)
type Program = [Chunk]

isDef chunk =
    case chunk of
    Def _ _ _ -> True
    Prose _ -> False

getName chunk =
    case chunk of
    Def _ name _ -> name
    _ -> error "cannot retrieve name, not a def"

getParts chunk =
    case chunk of
    Def _ _ parts -> parts
    _ -> error "cannot retrieve parts, not a def"

getLineNo chunk =
    case chunk of
    Def line _ _ -> line
    _ -> error "cannot retrieve line number, not a def"

getProseText chunk = 
    case chunk of
    Prose txt -> txt
    _ -> error "cannot retrieve text, not a prose"

