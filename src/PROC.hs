module PROC
  ( module PROC.Base
  , module PROC.Parsing
  , module PROC.Flow
  ) where

import PROC.Base
import PROC.Parsing
import PROC.Flow

example :: String
example = unlines
  [ "const(x,y) {"
  ,   "return x;"
  , "}"
  , "x = 10;"
  , "y = 11;"
  , "z = const(x,y);"
  ]