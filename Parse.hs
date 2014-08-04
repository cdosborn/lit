module Parse
( parse ) where

import Text.Parsec

data Chunk = Def String [Part] | Prose String
    deriving Show
data Part = Code String | Ref String
    deriving Show
data Prgrm = [Chunk]
    deriving Show

litFile = sepBy chunk eol
chunk = try prose 
    <|> def 

def = do 
    t <- title
    parts <- body
    return Def t parts
   
-- returns name from < name doesn't remove external spaces (sigh) >=
title :: Parser String
title = do
    x <- between (string "<<") (string ">>=") (many (space <|> alphaNum))
    eol
    return x
-- returns code body until next chunk
body = do
    x 

prose =  
    
eol = char '\n' <|> char '\r'
eof

parseLitProg :: String -> [Chunk] 
mergeSameDefs :: [Chunk] -> [Chunk]
expandLitProg :: Chunk -> Chunk
revertLitProg :: [Chunk] -> String 
