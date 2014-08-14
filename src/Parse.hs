{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Text.Regex
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T

import Types

encode :: T.Text -> [Chunk]
encode txt =
    case (parse entire "" txt) of 
    Left err -> []
    Right result -> result

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

entire :: Parser Program
entire = manyTill chunk eof

chunk :: Parser Chunk
chunk = (try def) <|> prose

--prose :: Parser Chunk
--prose = do 
--    txts <- manyTill grabLine beginDef
--    return $ Prose $ T.concat txts
prose :: Parser Chunk
prose = do 
    txt <- packM =<< many (noneOf "\n\r")
    nl <- eol >>= (\c -> return $ T.singleton c)
    return $ Prose (txt `T.append` nl)

def :: Parser Chunk
def = do
    (indent, header, lineNum) <- title
    parts <- manyTill (part indent) $ endDef indent
    return $ Def lineNum header parts

part :: String -> Parser Part
part indent =
    try (string indent >> varLine) <|> 
    try (string indent >> defLine) <|> 
    (grabLine >>= (\extra -> return (Code $ extra)))
  --(newline >>= (\nl -> return (Code $ T.singleton nl)))

varLine :: Parser Part
varLine = do
    name <- packM =<< between (string "<<") (string ">>") (many notDelim)
    eol
    return $ Ref name

defLine :: Parser Part
defLine = do
    line <- grabLine 
    return $ Code line

-- Post: Consume newlines between a Code Chunk's last line and a Prose
--endDef :: String -> Parser ()
--endDef indent = try (skipMany newline >> (notFollowedBy (string indent) <|> ((lookAhead title) >> parserReturn ())))

endDef :: String -> Parser ()
endDef indent = try $ do { skipMany newline; notFollowedBy (string indent) <|> (lookAhead title >> parserReturn ()) }

beginDef = try $ do {lookAhead title >> parserReturn ()}

grabLine :: Parser T.Text
grabLine = do 
    line <- packM =<< many (noneOf "\n\r")
    last <- eol >>= (\c -> return $ T.singleton c)
    return $ line `T.append` last

packM str = return $ T.pack str

-- Pre: Assumes that parser is looking at a fresh line with a macro defn
-- Post: Returns (indent, macro-name, line-no)
title :: Parser (String, T.Text, Int)
title = do
    pos <- getPosition
    indent <- many ws
    name <- packM =<< between (string "<<") (string ">>=") (many notDelim)
    eol
    return $ (indent, T.strip name, sourceLine pos)

notDelim = noneOf ">="
ws :: Parser Char
ws = char ' ' <|> char '\t'  -- consume a whitespace char
eol :: Parser Char
eol = char '\n' <|> char '\r'

fileNameFromPath :: String -> String
fileNameFromPath path =
    let r = mkRegex "(\\w+\\.\\w+)\\.lit$"
        m = matchRegex r path 
    in case m of 
        Just (fst:rest) -> fst
        Nothing -> ""
