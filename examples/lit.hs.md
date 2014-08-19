# lit.hs - a *modern* literate tool 

`lit` attempts to build upon the evolution of prior [literate programming](http://en.wikipedia.org/wiki/Literate_programming) (LP) tools. Donald Knuth pioneered LP and with it the first tool CWEB. CWEB was initially limited to only C and utilized 35 different control structures. CWEB supported tex for typesetting the literate programs. `lit`, while more restricted, is language independent with only 2 control structures. `lit` uses markdown for styling documents, and relies on indentation for defining code chunks.

A high-level overview of `lit.hs`:
```Haskell
<< * >>=
<< imports >>
<< types >>
<< usage messages >>
<< main >>
```
`lit` is a command line utility which relies upon several libraries which handle standard command line use. `lit` imports `Processing` which is the glue for the programs other components. `Poll` is the module which handles file polling in the `--watch` option available in `lit`.
```Haskell
<< imports >>=
module Main where

import System.Console.GetOpt
import System.Environment
import System.Directory (doesDirectoryExist)
import System.IO
import System.Exit
import Control.Applicative

import Processing
import Poll
```
`types` defines the supported command line options and functions used to handle these options
```Haskell
<< types >>=
<< datatype for passing options >>
<< default options >>
<< option transforms >>
```
`Options` serves to represent the different command line options which are handled during use. 
```Haskell
<< datatype for passing options >>=
data Options = Options  { optCodeDir  :: String 
                        , optDocsDir  :: String
                        , optCss      :: Maybe String
                        , optCode     :: Bool
                        , optHtml     :: Bool
                        , optMarkdown :: Bool
                        , optWatch    :: Bool
                        }
```
```Haskell
<< default options >>=
startOptions :: Options
startOptions = Options  { optCodeDir  = "./"
                        , optDocsDir  = "./"
                        , optCss      = Nothing
                        , optCode     = False
                        , optHtml     = False
                        , optMarkdown = False
                        , optWatch    = False
                        }
```
`options` is a list of monadic functions, which are applied to the options passed as arguments to `lit`. `options` provides the general program flow for argument handling.
```Haskell
<< option transforms >>=
options :: [ OptDescr (Options -> IO Options) ]
options = 
    [ Option  "h" ["html"]
       (NoArg (\opt -> return opt { optHtml = True }))
       "Generate html"

    , Option "m" ["markdown"]
       (NoArg (\opt -> return opt { optMarkdown = True }))
       "Generate markdown"

    , Option "c" ["code"]
       (NoArg (\opt -> return opt { optCode = True }))
       "Generate code by file extension"

    , Option "" ["css"]
       (ReqArg
           (\arg opt -> return opt { optCss = Just arg })
           "FILE")
       "Specify a css file for html generation"

    , Option "" ["docs-dir"]
       (ReqArg
           (\arg opt -> return opt { optDocsDir = arg })
           "DIR")
       "Directory for generated docs"

    , Option "" ["code-dir"]
       (ReqArg
           (\arg opt -> return opt { optCodeDir = arg })
           "DIR")
       "Directory for generated code"

    , Option "w" ["watch"]
       (NoArg
           (\opt -> return opt { optWatch = True}))
       "Watch for file changes, automatically run lit"
 
    , Option "v" ["version"]
       (NoArg
           (\_ -> do
               hPutStrLn stderr "Version 0.01"
               exitWith ExitSuccess))
       "Print version"
 
    , Option "" ["help"]
       (NoArg
           (\_ -> do
               prg <- getProgName
               hPutStrLn stderr (usageInfo usage options)
               exitWith ExitSuccess))
       "Display help"
    ]
```
`usage` and `help` are definitions used whenever `--help` is passed, or an incorrect option is passed.
```Haskell
<< usage messages >>=
usage = "Usage: lit OPTIONS... FILES..."
help = "Try:   lit --help"
```
`main` defines the entry point for execution. `getOpt` interprets the command line arguments to yield `actions`, `files`, and `errors`
```Haskell
<< main >>=
main = do
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, files, errors) = getOpt Permute options args
```
After interpreting the arguments, `foldl` threads the default options through each action, updating the defaults.
```Haskell
<< main >>=
    opts <- foldl (>>=) (return startOptions) actions
 
    let Options { optCodeDir  = codeDir
                , optDocsDir  = docsDir
                , optMarkdown = markdown
                , optCode     = code
                , optHtml     = html
                , optCss      = mCss
                , optWatch    = watching
                } = opts 
```
A brief check to ensure that the directories actually exist for the given strings. This prevents ambiguous errors about writing to invalid paths.
```Haskell
<< main >>=
    codeDirCheck <- doesDirectoryExist codeDir
    docsDirCheck <- doesDirectoryExist docsDir
```
Based on the conditions, we build part of the pipeline for generating (html/markdown/code). This block aims to gather all the pipes and program errors into lists, which will be iterated over. `maybeWatch` determines whether the files should be directly passed through the pipes via `mapM_` or whether `Poll.watch` should manage the pipelines whenever the underlying files change.
```Haskell
<< main >>=
    let htmlPipe = if html     then [Processing.htmlPipeline docsDir mCss] else []
        mdPipe   = if markdown then [Processing.mdPipeline   docsDir mCss] else []
        codePipe = if code     then [Processing.codePipeline codeDir mCss] else []
        pipes = htmlPipe ++ mdPipe ++ codePipe 
        maybeWatch = if watching then Poll.watch else mapM_
        errors'  = if codeDirCheck then [] else ["Directory: " ++ codeDir ++ " does not exist\n"]
        errors'' = if docsDirCheck then [] else ["Directory: " ++ docsDir ++ " does not exist\n"]
        allErr = errors ++ errors' ++ errors''
```
In the final call of main, either the programs prints [errors](#usage%20messages) or redirects the program flow with `maybeWatch`. The bulk of the file processing is passed to the `Processing.hs` module.
```Haskell
<< main >>=
    if allErr /= [] || (not html && not code && not markdown) || files == []
        then hPutStrLn stderr ((concat allErr) ++ help) 
        else (maybeWatch (Processing.build pipes)) files
```
