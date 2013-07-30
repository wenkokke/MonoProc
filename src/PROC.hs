module PROC
  ( module PROC.Base
  , module PROC.Parsing
  , module PROC.Evaluating
  , module PROC.MF
  , mkProg , runProg
  , ex1 , ex2 , ex3 , ex4 , ex5 , fib
  ) where

import Prelude hiding (init)
import PROC.Base
import PROC.Parsing
import PROC.Evaluating
import PROC.MF
import qualified Data.Map as M
import Text.ParserCombinators.UU.Utils (runParser)

parseProg  = runLabel . runParser "stdin" pProg
parseDecl  = runLabel . runParser "stdin" pDecl
parseStmt  = runLabel . runParser "stdin" pStmt

mkProg :: [String] -> Prog
mkProg = parseProg . unlines

runProg :: Prog -> Maybe Integer
runProg p = either (const Nothing) (M.lookup "return") (evalProg p)

ex1, ex2, ex3, ex4, ex5 :: Prog

ex1 = mkProg
  [ "x = 0;"
  , "while (x < 10) {"
  , "  x = x + 1;"
  , "}"
  ]

ex2 = mkProg
  [ "add(a,b) {"
  , "  return a + b;"
  , "}"
  , "add(3,5);"
  ]

ex3 = mkProg
  [ "next() {"
  , "  x = x + 1;"
  , "}"
  , "x = 0;"
  , "while (x < 5) {"
  , "  next();"
  , "}"
  ]

ex4 = mkProg
  [ "mask(a) {"
  , "  a = a + 1;"
  , "  return a;"
  , "}"
  , "a = 10;"
  , "mask(a);"
  ]

ex5 = mkProg
  [ "fib(z,u) {"
  , "  if (z < 3) {"
  , "    return = u + 1;"
  , "  }"
  , "  else {"
  , "    fib(z-1,u);"
  , "    fib(z-2,return);"
  , "  }"
  , "  skip;"
  , "}"
  , "fib(5,0);"
  ]

fib :: Integer -> Prog
fib n = mkProg
  [ "fib(n,r) {"
  , "  if (n <= 2) {"
  , "    return r + 1;"
  , "  }"
  , "  else {"
  , "    r = fib(n - 1,r);"
  , "    r = fib(n - 2,r);"
  , "    return r;"
  , "  }"
  , "}"
  , "fib("++show n++",0);"
  ]
