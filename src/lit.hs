module Main where

import System.Console.GetOpt
import System.Environment
import System.Directory (doesDirectoryExist)
import System.IO
import System.Exit
import Control.Applicative

import Process
import Poll
data Options = Options  { optCodeDir  :: String
                        , optDocsDir  :: String
                        , optCss      :: Maybe String
                        , optCode     :: Bool
                        , optHtml     :: Bool
                        , optMarkdown :: Bool
                        , optWatch    :: Bool
                        , optNumber   :: Bool
                        }
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
       "Add annotations to generated code noting the source lit file and line number"

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
               hPutStrLn stderr "Version 0.1.1.0"
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
usage = "Usage: lit OPTIONS... FILES..."
help = "Try:   lit --help"
main = do
    args <- getArgs

    -- Parse options, getting a list of option actions
    let (actions, files, errors) = getOpt Permute options args
    opts <- foldl (>>=) (return startOptions) actions

    let Options { optCodeDir  = codeDir
                , optDocsDir  = docsDir
                , optMarkdown = markdown
                , optCode     = code
                , optHtml     = html
                , optCss      = mCss
                , optWatch    = watching
                , optNumber   = showLines
                } = opts
    codeDirCheck <- doesDirectoryExist codeDir
    docsDirCheck <- doesDirectoryExist docsDir
    let htmlPipe = if html     then [Process.htmlPipeline docsDir mCss] else []
        mdPipe   = if markdown then [Process.mdPipeline   docsDir mCss] else []
        codePipe = if code     then [Process.codePipeline codeDir mCss showLines] else []
        pipes = htmlPipe ++ mdPipe ++ codePipe
        maybeWatch = if watching then Poll.watch else mapM_
        errors'  = if codeDirCheck then [] else ["Directory: " ++ codeDir ++ " does not exist\n"]
        errors'' = if docsDirCheck then [] else ["Directory: " ++ docsDir ++ " does not exist\n"]
        allErr = errors ++ errors' ++ errors''
    if allErr /= [] || (not html && not code && not markdown) || files == []
        then hPutStrLn stderr ((concat allErr) ++ help)
        else (maybeWatch (Process.process pipes)) files
