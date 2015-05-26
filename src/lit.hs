{-# LINE 7 "src/lit.hs.lit" #-}
{-# LINE 14 "src/lit.hs.lit" #-}
module Main where

import System.Console.GetOpt
import System.Environment
import System.Directory (doesDirectoryExist)
import System.IO
import System.Exit
import Control.Applicative

import Process
import Poll
{-# LINE 29 "src/lit.hs.lit" #-}
{-# LINE 35 "src/lit.hs.lit" #-}
data Options = Options  { optCodeDir  :: String 
                        , optDocsDir  :: String
                        , optCss      :: Maybe String
                        , optCode     :: Bool
                        , optHtml     :: Bool
                        , optMarkdown :: Bool
                        , optWatch    :: Bool
                        , optNumber   :: Bool
                        }
{-# LINE 46 "src/lit.hs.lit" #-}
startOptions :: Options
startOptions = Options  { optCodeDir  = "./"
                        , optDocsDir  = "./"
                        , optCss      = Nothing
                        , optCode     = False
                        , optHtml     = False
                        , optMarkdown = False
                        , optWatch    = False
                        , optNumber   = False
                        }
{-# LINE 59 "src/lit.hs.lit" #-}
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

    , Option "n" ["number"]
       (NoArg (\opt -> return opt { optNumber = True }))
       "Add annotations to generated code noting the lit file and line from which it came"

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
               hPutStrLn stderr "Version 0.1.0.9"
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
{-# LINE 118 "src/lit.hs.lit" #-}
usage = "Usage: lit OPTIONS... FILES..."
help = "Try:   lit --help"
{-# LINE 123 "src/lit.hs.lit" #-}
main = do
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, files, errors) = getOpt Permute options args
{-# LINE 131 "src/lit.hs.lit" #-}
    opts <- foldl (>>=) (return startOptions) actions
 
    let Options { optCodeDir  = codeDir
                , optDocsDir  = docsDir
                , optMarkdown = markdown
                , optCode     = code
                , optHtml     = html
                , optCss      = mCss
                , optWatch    = watching
                , optNumber   = numberLines
                } = opts 
{-# LINE 145 "src/lit.hs.lit" #-}
    codeDirCheck <- doesDirectoryExist codeDir
    docsDirCheck <- doesDirectoryExist docsDir
{-# LINE 150 "src/lit.hs.lit" #-}
    let htmlPipe = if html     then [Process.htmlPipeline docsDir mCss numberLines] else []
        mdPipe   = if markdown then [Process.mdPipeline   docsDir mCss numberLines] else []
        codePipe = if code     then [Process.codePipeline codeDir mCss numberLines] else []
        pipes = htmlPipe ++ mdPipe ++ codePipe 
        maybeWatch = if watching then Poll.watch else mapM_
        errors'  = if codeDirCheck then [] else ["Directory: " ++ codeDir ++ " does not exist\n"]
        errors'' = if docsDirCheck then [] else ["Directory: " ++ docsDir ++ " does not exist\n"]
        allErr = errors ++ errors' ++ errors''
{-# LINE 161 "src/lit.hs.lit" #-}
    if allErr /= [] || (not html && not code && not markdown) || files == []
        then hPutStrLn stderr ((concat allErr) ++ help) 
        else (maybeWatch (Process.process pipes)) files
