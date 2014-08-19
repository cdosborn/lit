module Util where

import qualified Data.Text as T

ensureTrailingSlash dir = 
    if last dir == '/'
    then dir
    else dir ++ "/"

(<++>) :: T.Text -> T.Text -> T.Text
(<++>) = T.append
