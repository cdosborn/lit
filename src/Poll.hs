module Poll 
( watch ) where

import System.Directory
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad (forever)
import qualified Control.Concurrent as C
import System.IO.Error
 
watch :: (String -> IO ()) -> [String] -> IO ()
watch fun fs = do 
    putStrLn "starting.."
    mapM_ fun fs
     -- total microseconds for each file to cause a 1 sec delay per loop
    let delay = 1000000 `div` (length fs)
    forever $ mapM_ (onDiff fun delay) fs

onDiff :: (String -> IO ()) -> Int -> String -> IO ()
onDiff fun delay file = do
    modified <- errorHandler (getModificationTime file) 
    curTime <- getCurrentTime 
    let diff = (diffUTCTime curTime modified)
    if diff < 1 then fun file >> C.threadDelay delay else return ()
 

-- a really conservative check to prevent file
-- "inavailability" due to reading modification bits
errorHandler = errorHandlerNTimes 100

errorHandlerNTimes 0 mnd = catchIOError mnd (\e -> ioError e)
errorHandlerNTimes times mnd = catchIOError mnd handle 
    where   
        handle e =
            if isDoesNotExistError e 
            then C.threadDelay 5000 >> errorHandlerNTimes (times - 1) mnd
            else ioError e
