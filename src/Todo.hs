module Todo where

import Text.Read

data Todo = Todo

instance Read Todo where
    readPrec = undefined