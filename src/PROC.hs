module PROC
  ( module PROC.Base
  , module PROC.Parsing
  , module PROC.MF
  , mkProg
  ) where

import Prelude hiding (init)
import PROC.Base
import PROC.Parsing
import PROC.MF
import Text.ParserCombinators.UU.Utils (runParser)

parseProg  = runLabel . runParser "stdin" pProg
parseDecl  = runLabel . runParser "stdin" pDecl
parseStmt  = runLabel . runParser "stdin" pStmt

mkProg :: [String] -> Prog
mkProg = parseProg . unlines

ex1 = mkProg
  [ "fib(a,b) {"
  , "  if (a < 3) {"
  , "    return b + 1;"
  , "  }"
  , "  else {"
  , "    f1 = fib(a - 1,b);"
  , "    f2 = fib(a - 2,f1);"
  , "    return f2;"
  , "  }"
  , "}"
  ]
