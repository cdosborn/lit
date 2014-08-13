## Lit - a *modern* literate tool 

 *A fancy quote seems appropriate here*
as

Here is a broad overview of the programs
```Haskell
<< * >>=
<< <a href="#imports">imports</a> >>
<< <a href="#command options">command options</a> >>
<< <a href="#command redirects">command redirects</a> >>
<< <a href="#main">main</a> >>

```
  
We need to import some code for working on the command line
```Haskell
<< imports >>=
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Processing

```
    
Now we define the data behind some command line options
```Haskell
<< command options >>=
data Options = Options  { optCodeDir  :: String 
                        , optHtmlDir  :: String
                        , optCodeOnly :: Bool
                        , optHtmlOnly :: Bool
                        }

startOptions :: Options
startOptions = Options  { optCodeDir  = "./"
                        , optHtmlDir  = "./"
                        , optCodeOnly = False
                        , optHtmlOnly = False
                        }

```
Now we define the control flow for programs
```Haskell
<< command redirects >>=
options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "d" ["html-only"]
        (NoArg (\opt -> return opt { optHtmlOnly = True }))
        "Generate html docs"
      
     , Option "" ["html-dir"]
        (ReqArg
            (\arg opt -> return opt { optCodeDir = arg })
            "DIR")
        "Directory for generated html"

    , Option "c" ["code-only"]
        (NoArg (\opt -> return opt { optCodeOnly = True }))
        "Generate code by file extension"

     , Option "" ["code-dir"]
        (ReqArg
            (\arg opt -> return opt { optCodeDir = arg })
            "DIR")
        "Directory for generated code"
 
    , Option "v" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo header options)
                exitWith ExitSuccess))
        "Display help"
    ]

header = "Usage: lit [OPTION...] FILES..."

```
Here is the starting point of the program
```Haskell
<< main >>=
main = do

```
Here we grab some arguments as a monad
```Haskell
<< main >>=
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, files, errors) = getOpt RequireOrder options args
    
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions
 
    let Options { optCodeDir  = codeDir
                , optHtmlDir  = htmlDir
                , optCodeOnly = onlyCode
                , optHtmlOnly = onlyHtml
                } = opts

```
Lastly we either generate code or html or both
```Haskell
<< main >>=
    if (errors /= [] || (onlyHtml && onlyCode))
    then hPutStrLn stderr ((concat errors) ++ header)
    else if onlyCode         
    then hPutStrLn stderr "... not done"
    else if onlyHtml
    then hPutStrLn stderr "... not done"
    else Processing.buildAll codeDir htmlDir files
--       Prose.write pathToSource pathToOutput files

```
