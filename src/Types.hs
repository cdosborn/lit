module Types where

import Data.Text 

data Chunk = Def Int Text [Part] | Prose Text deriving (Show)
data Part = Code Text | Ref Text deriving (Show)
type Program = [Chunk]
