--module Test 
--(..) where

import Text.Parsec
import Text.Parsec.String
import Parse
{- 
 - TODO:
 - allow more within a title right now only accepts "0-9a-zA-Z"
 - allow << ... >>=\n to have whitespace before \n
 - reduce unnecessary parens in methods (title)
 -}

--main = parseTest title "\t\t  << asddf df >>=\n"
--main = parseTest (many takeLine) "asfdasdf\rasasf\n"
--main = parseTest skipLine "asfdasdf\rasasf\n"
--main = parseTest (skipLine >> many anyChar) "\n\nasfdasdf\rasasf\n"
--main = parseTest (defLine "    ") "    asfdasdf\rasasf\n"
--main = parseTest (manyTill takeLine (try (difIndent "    "))) "    asdfd\n  asasf\nasdf"
--main = parseTest def "    << asddf df >>=\n    asdfd\n  asasf\nasdf"
--main = parseTest (many chunk) "    << asddf df >>=\n    asdfd\n  asasf\nasdf\n<< asddf df >>=\nasdfd\n"
--main = parseTest entire "    << asddf df >>=\n    asdfd\n  asasf\nasdf\n<< asddf df >>=\nasdfd\n"
--main = parseTest (skipMany newline >> endDef  "    " >> (many anyChar)) "\n\n\n         << asddf df >>=\n\n    asdfd\n  asasf\nasdf"
--main = parseTest ((manyTill grabLine (try (endDef "    "))) >> many anyChar) "    asdfd\n  asasf\nasdf"
--main = parseTest (part "  ") "  << asddf df >>\n"
--main = parseTest def "  << asddf df >>=\n  asdfd\n  asasf"
main = parseFromFile entire "lit-docs/ab.oo.lit"

--parseFromFile p fname = do
--    input <- readFile fname 
--    return (runParser p () fname input)
