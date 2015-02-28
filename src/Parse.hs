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

endDef :: String -> Parser ()
endDef indent = try $ do { skipMany newline; notFollowedBy (string indent) <|> (lookAhead title >> parserReturn ()) }

-- Returns (indent, macro-name, line-no)
title :: Parser (String, T.Text, Int)
title = do
    pos <- getPosition
    indent <- many ws
    name <- packM =<< between (string "<<") (string ">>=") (many notDelim)
    newline
    return $ (indent, T.strip name, sourceLine pos)

notDelim = noneOf ">="

part :: String -> Parser Part
part indent = 
    try (string indent >> varLine) <|> 
    try (string indent >> defLine) <|>
    (grabLine >>= \extra -> return $ Code extra)

varLine :: Parser Part
varLine = do
    indent <- packM =<< many ws
    name <- packM =<< between (string "<<") (string ">>") (many notDelim)
    newline
    return $ Ref name indent

defLine :: Parser Part
defLine = do
    line <- grabLine
    return $ Code line

grabLine :: Parser T.Text
grabLine = do 
    line <- many (noneOf "\n\r")
    last <- newline
    return $ T.pack $ line ++ [last]

ws :: Parser Char
ws = char ' ' <|> char '\t'


packM str = return $ T.pack str

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


