module Main where

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Control.Applicative

import Processing
import Poll

data Options = Options  { optCodeDir  :: String 
                        , optHtmlDir  :: String
                        , optCodeOnly :: Bool
                        , optHtmlOnly :: Bool
                        , optCss      :: Maybe String
                        , optWatch    :: Bool
                        }

startOptions :: Options
startOptions = Options  { optCodeDir  = "./"
                        , optHtmlDir  = "./"
                        , optCss      = Nothing
                        , optCodeOnly = False
                        , optHtmlOnly = False
                        , optWatch    = False
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "d" ["html-only"]
        (NoArg (\opt -> return opt { optHtmlOnly = True }))
        "Generate html docs"
      
     , Option "" ["html-dir"]
        (ReqArg
            (\arg opt -> return opt { optHtmlDir = arg })
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

     , Option "" ["css"]
        (ReqArg
            (\arg opt -> return opt { optCss = Just arg })
            "FILE")
        "Specify a css file for html generation"

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
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo header options)
                exitWith ExitSuccess))
        "Display help"
    ]

header = "Usage: lit [OPTION...] FILES..."

main = do
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, files, errors) = getOpt Permute options args
    
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions
 
    let Options { optCodeDir  = codeDir
                , optHtmlDir  = htmlDir
                , optCodeOnly = onlyCode
                , optHtmlOnly = onlyHtml
                , optCss      = mCss
                , optWatch    = watching
                } = opts 

    let build = if onlyHtml then Processing.buildHtml mCss htmlDir else Processing.buildCode codeDir
        build' = if not onlyHtml && not onlyCode then Processing.buildAll mCss codeDir htmlDir else build
        maybeWatch = if watching then Poll.watch else mapM_
    if (errors /= [] || (onlyHtml && onlyCode)) 
        then hPutStrLn stderr ((concat errors) ++ header) 
        else maybeWatch build' files  
