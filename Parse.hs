module Parse where

import Text.Parsec
import Text.Parsec.String

data Chunk = Def String [Part] | Prose String deriving (Show)
data Part = Code String | Ref String deriving (Show)
type Program = [Chunk]

--parseLitProg :: String -> [Chunk] 
--mergeSameDefs :: [Chunk] -> [Chunk]
--expandLitProg :: Chunk -> Chunk
--revertLitProg :: [Chunk] -> String 

--build outputDir files = 
--    let outputPaths = map (\f -> outputDir ++ (fileNameFromPath f)) files 
--    in mapM readFile files >>= (sequence . zipWith process outputPaths) >> return ()
--
--process out src =
--    case (parse entire "" src) of 
--    Left err -> putStrLn (show err) >> (return ())
--    Right result -> writeFile out result >> (return ())
--
--fileNameFromPath :: String -> String
--fileNameFromPath path =
--    let r = mkRegex "(\\w+\\.\\w+)\\.lit$"
--        m = matchRegex r path 
--    in case m of 
--        Just (fst:rest) -> fst
--        Nothing -> ""

entire :: Parser Program
entire = manyTill chunk eof

chunk :: Parser Chunk
chunk = (try def) <|> prose

prose :: Parser Chunk
prose = do 
    str <- many (noneOf "\n\r")
    nl <- eol 
    return $ Prose (str ++ [nl])

def :: Parser Chunk
def = do
    (indent, header) <- title
    nls <- many newline
    parts <- manyTill (part indent) (endDef indent)
    return $ Def header parts

part indent = try (string indent >> varLine) <|> try defLine <|> (many newline >>= (\nls -> return (Code $ nls)))

varLine :: Parser Part
varLine = do
    name <- between (string "<<") (string ">>") (many notDelim)
    eol
    return $ Ref name

-- Post: Returns a code part, defLine is a misnomer, as there could be multiple lines
defLine :: Parser Part
defLine = do
    line <- grabLine 
    nls <- many newline
    return $ Code (line ++ nls)

-- Post: Consume newlines between a Code Chunk's last line and a Prose
endDef :: String -> Parser ()
endDef indent = try (skipMany newline >> (notFollowedBy (string indent) <|> ((lookAhead title) >> parserReturn ())))

grabLine :: Parser String
grabLine = do 
    many <- many (noneOf "\n\r")
    last <- eol
    return $ many ++ [last]

-- Pre: Assumes that parser is looking at a fresh line with a macro defn
-- Post: Returns (indent, macro-name@line-no\n)
title :: Parser (String, String)
title = do
    pos <- getPosition
    indent <- many ws
    name <- between (string "<<") (string ">>=") (many notDelim)
    eol
    return $ (indent, name ++ "@" ++ (show (sourceLine pos)) ++ "\n")

notDelim = noneOf ">="
ws = space <|> char '\t'  -- consume a whitespace char
eol = char '\n' <|> char '\r'
