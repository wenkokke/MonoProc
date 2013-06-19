module PROC
  ( module PROC.Base
  , module PROC.Parsing
  ) where

import PROC.Base
import PROC.Parsing

example :: String
example = unlines
  [ "const(x,y) {"
  ,   "return x;"
  , "}"
  ]