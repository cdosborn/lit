{-# LINE 7 "src/Types.hs.lit" #-}
{-# LINE 14 "src/Types.hs.lit" #-}
module Types where

import Data.Text 
{-# LINE 26 "src/Types.hs.lit" #-}
data SourceLoc = SourceLoc String Int deriving (Show, Eq)
data Chunk = Def SourceLoc Text [Part] | Include String Text | Prose Text deriving (Show, Eq)
data Part = Code Text | Ref Text Text deriving (Show, Eq)
type Program = [Chunk]
{-# LINE 33 "src/Types.hs.lit" #-}
isDef chunk =
    case chunk of
    Def _ _ _ -> True
    Include _ _ -> False
    Prose _ -> False
{-# LINE 41 "src/Types.hs.lit" #-}
isRef part =
    case part of
    Ref _ _ -> True
    _ -> False
{-# LINE 48 "src/Types.hs.lit" #-}
getName chunk =
    case chunk of
    Def _ name _ -> name
    _ -> error "cannot retrieve name, not a def"
{-# LINE 55 "src/Types.hs.lit" #-}
getCodeText part = 
    case part of
    Code txt -> txt
    _ -> error "cannot retrieve text, not a code part"
{-# LINE 63 "src/Types.hs.lit" #-}
getParts chunk =
    case chunk of
    Def _ _ parts -> parts
    _ -> error "cannot retrieve parts, not a def"
{-# LINE 71 "src/Types.hs.lit" #-}
getLineNo chunk =
    case chunk of
    Def line _ _ -> line
    _ -> error "cannot retrieve line number, not a def"
{-# LINE 78 "src/Types.hs.lit" #-}
getProseText chunk = 
    case chunk of
    Prose txt -> txt
    _ -> error "cannot retrieve text, not a prose"
