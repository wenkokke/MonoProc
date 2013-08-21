module PROC
  ( module PROC.Base
  , module PROC.Parsing
  , module PROC.Evaluating
  , module PROC.MF
  , mkProg , runProg
  , ex1 , ex2 , ex3 , ex4 , ex5 , ex6 , fib
  ) where

import Prelude hiding (init)
import PROC.Base
import PROC.Parsing
import PROC.Evaluating
import PROC.MF
import qualified Data.Map as M

mkProg :: [String] -> Prog
mkProg = parseProg . unlines

runProg :: Prog -> Maybe Integer
runProg p = either (const Nothing) (M.lookup "return") (evalProg p)

ex1 = mkProg
  [ "x = 0;"
  , "while (x < 10) {"
  , "  x = x + 1;"
  , "}"
  ]

ex2 = mkProg
  [ "add(a,b) {"
  , "  a = a + (x + y);"
  , "  return a + b;"
  , "}"
  , " "
  , "x = 10;"
  , "y = x + 10;"
  , "z = add(y,y + y);"
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
  , "  a = a + 2;"
  , "  a = a + 3;"
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

ex6 = mkProg
  [ "x = 2;"
  , "y = 4;"
  , "x = 1;"
  , "if (y > x) {"
  , "  z = y;"
  , "}"
  , "else {"
  , "  z = y * y;"
  , "}"
  , "x = z;"
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
