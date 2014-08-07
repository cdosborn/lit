import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit

import Processing

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

main = do
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

    if (errors /= [] || (onlyHtml && onlyCode))
    then hPutStrLn stderr ((concat errors) ++ header)
    else if onlyCode         
    then hPutStrLn stderr "... not done"
    else if onlyHtml
    then hPutStrLn stderr "... not done"
    else Processing.buildAll codeDir htmlDir files
--       Prose.write pathToSource pathToOutput files
