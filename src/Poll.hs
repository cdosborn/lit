module Poll 
( watch ) where

import System.Directory
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad (forever)
import qualified Control.Concurrent as C
import System.IO.Error

watch :: (String -> IO ()) -> [String] -> IO ()
watch fun fs = 
    let 
        wait = C.threadDelay 1000000
    in do 
    putStrLn "starting.."
    mapM_ fun fs
    forever $ (wait >> mapM_ (onChange fun) fs)

onChange :: (String -> IO ()) -> String -> IO ()
onChange fun file = do
    modified <- retryAtMost 10 (getModificationTime file) 
    curTime <- getCurrentTime 
    let diff = (diffUTCTime curTime modified)
    if diff < 2 then fun file else return ()

retryAtMost 1 action = catchIOError action (\e -> ioError e)
retryAtMost times action = 
    let
        handle e = if isDoesNotExistError e 
            then C.threadDelay 50000 >> retryAtMost (times - 1) action
            else ioError e
    in 
        catchIOError action handle 
