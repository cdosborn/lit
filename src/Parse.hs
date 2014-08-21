{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T

import Types

encode :: T.Text -> [Chunk]
encode txt =
    case (parse entire "" txt) of 
    Left err -> []
    Right result -> result

entire :: Parser Program
entire = manyTill chunk eof

chunk :: Parser Chunk
chunk = (try def) <|> prose

prose :: Parser Chunk
prose = grabLine >>= (\line -> return $ Prose line)

def :: Parser Chunk
def = do
    (indent, header, lineNum) <- title
    parts <- manyTill (part indent) $ endDef indent
    return $ Def lineNum header parts

part :: String -> Parser Part
part indent = 
    try (string indent >> varLine) <|> 
    (many ws >> grabLine >>= \extra -> return $ Code extra)

varLine :: Parser Part
varLine = do
    name <- packM =<< between (string "<<") (string ">>") (many notDelim)
    newline
    return $ Ref name

endDef :: String -> Parser ()
endDef indent = try $ do { skipMany newline; notFollowedBy (string indent) <|> (lookAhead title >> parserReturn ()) }

grabLine :: Parser T.Text
grabLine = do 
    line <- many (noneOf "\n\r")
    last <- newline
    return $ T.pack $ line ++ [last]

packM str = return $ T.pack str

-- Pre: Assumes that parser is looking at a fresh line with a macro defn
-- Post: Returns (indent, macro-name, line-no)
title :: Parser (String, T.Text, Int)
title = do
    pos <- getPosition
    indent <- many ws
    name <- fmap T.pack $ between (string "<<") (string ">>=") (many notDelim)
    newline
    return $ (indent, T.strip name, sourceLine pos)

notDelim = noneOf ">="
ws :: Parser Char
ws = char ' ' <|> char '\t'

textP :: Parsec T.Text () T.Text ->  T.Text -> T.Text
textP p txt =
    case (parse p "" txt) of 
    Left err -> T.empty
    Right result -> result

chunkP :: Parsec T.Text () Chunk ->  T.Text -> Maybe Chunk
chunkP p txt =
    case (parse p "" txt) of 
    Left err -> Nothing
    Right result -> Just result
