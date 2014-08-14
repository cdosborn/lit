{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Test.HUnit
import Types
import Parse

-- Parse 
testGrabLine = TestCase $ assertEqual 
    "Should accept 0+ anyChar followed by newline" "\n" (textP grabLine "\n")

testDef = TestCase $ assertEqual 
    "Simplest Def chunk" 
    [Def 1 "asdf" [Code "ass\n",Code "sd\n",Code " 4\n"]] $
    encode "  << asdf >>=\n  ass\n  sd\n   4\n"

testDefWithNewLines = TestCase $ assertEqual 
    "Def chunk with newlines in between code" 
    [Def 1 "asdf" [Code "ass\n",Code "\n",Code "\n",Code "sd\n",Code " 4\n"]] $
    encode "  << asdf >>=\n  ass\n\n\n  sd\n   4\n"

testDefFollowedByTitle = TestCase $ assertEqual 
    "Def chunk followed by chunk" 
    [Def 1 "asdf" [Code "ass\n"],Def 3 "bb" []] $ 
    encode "  << asdf >>=\n  ass\n  << bb >>=\n"

testProse = TestCase $ assertEqual 
    "Prose with newlines" 
    [Prose "asdfasd\n", Prose "\n", Prose "  asdf\n",Def 4 "asdf" []] $
    encode "asdfasd\n\n  asdf\n  <<asdf>>=\n"

testProseOnlyNL = TestCase $ assertEqual 
    "Prose with newlines" 
    [Prose "\n",Prose "\n",Prose "\n"] $
    encode "\n\n\n"


testDefPrecedeWithNL = TestCase $ assertEqual 
    "Def preceded by newlines" 
    [Prose "\n",Prose "\n",Prose "\n",Def 4 "asdf" [Code "durp\n"]] $
    encode "\n\n\n  <<asdf>>=\n  durp\n"

-- Processing
-- Pretty
-- Parse

main = runTestTT $ TestList 
    [ testGrabLine, testTitle
    , testDef, testDefFollowedByTitle, testDefWithNewLines, testDefPrecedeWithNL 
    , testProse, testProseOnlyNL ]
