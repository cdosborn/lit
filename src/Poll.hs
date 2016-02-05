{-# LINE 7 "src/Poll.hs.lit" #-}
{-# LINE 15 "src/Poll.hs.lit" #-}
module Poll 
( watch ) where
{-# LINE 19 "src/Poll.hs.lit" #-}
import System.Directory
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad (forever)
import qualified Control.Concurrent as C
import System.IO.Error
{-# LINE 30 "src/Poll.hs.lit" #-}
watch :: (String -> IO ()) -> [String] -> IO ()
watch fun fs = 
    let 
        wait = C.threadDelay 1000000
    in do 
    putStrLn "starting.."
    mapM_ fun fs
    forever $ (wait >> mapM_ (onChange fun) fs)
{-# LINE 43 "src/Poll.hs.lit" #-}
onChange :: (String -> IO ()) -> String -> IO ()
onChange fun file = do
    modified <- retryAtMost 10 (getModificationTime file) 
    curTime <- getCurrentTime 
    let diff = (diffUTCTime curTime modified)
    if diff < 2 then fun file else return ()
{-# LINE 55 "src/Poll.hs.lit" #-}
retryAtMost 1 action = catchIOError action (\e -> ioError e)
retryAtMost times action = 
    let
        handle e = if isDoesNotExistError e 
            then C.threadDelay 50000 >> retryAtMost (times - 1) action
            else ioError e
    in 
        catchIOError action handle 
