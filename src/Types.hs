module Types where

import Data.Text 

data Chunk = Def Int Text [Part] | Prose Text deriving (Show, Eq)
data Part = Code Text | Ref Text deriving (Show, Eq)
type Program = [Chunk]
