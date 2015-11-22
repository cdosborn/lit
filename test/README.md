##To run output tests
`../scripts/test.sh`

##To add output tests
Add two files: issue42, issue42.lit. Where issue42 is the desired output of
issue42.lit. 

##A test failed!
test.sh will create a results directory where errors are
reported as diffs of what was expected. 

##Run haskell tests
`ghc -i../src/ Tests.hs -e 'Tests.main'`
