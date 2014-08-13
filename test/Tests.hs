{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Test.HUnit
import Types
import Parse

-- Parse 
testGrabLine = TestCase $ assertEqual
  "Should accept anyChar followed by newline" "\n" (textP grabLine "\n")

testTitle = TestCase $ assertEqual
  "Should match title" "a"  (textP durp "  << a >>=  \n")

testNewlineFile = TestCase $ assertEqual
  "Should handle simple newline file" (Just (Prose "\n")) (chunkP prose "asdfasd\nasdfasd\n  << asdf >>=\n")

testDef = TestCase $ assertEqual 
  "Should ignore extra \\n after code def" [(Def 1 "asdf" [Code "asdf\n"])] (encode "  << asdf >>=\n  asdf\n\n\n")

testProse = TestCase $ assertEqual 
  "Simplest code chunk" (Just (Prose "\n\n\n")) (chunkP chunk "  << asdf >>=\n  ass\n  sd\n    4")

testProseEntire = TestCase $ assertEqual 
  "Should not ignore \\n" [(Prose "\n\n\n")] (encode "asdfasd\n  <<asdf>>=\n")


-- Processing
-- Pretty
-- Parse

main = runTestTT $ TestList [testGrabLine, testNewlineFile, testDef, testProse, testProseEntire, testTitle]
