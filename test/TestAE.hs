module Main where

import PROC
import PROC.MF.Analysis.AE
import PROC.Testing (testAnalysis)
import PROC.Parsing (pAExprSet)

import Data.Set (Set)
import Text.ParserCombinators.UU.Utils (runParser)
  
main :: IO ()
main = do testAnalysis mfp mfAE progAE reslAE
          testAnalysis (mopk 3) mfAE progAE reslAE
    
progAE :: Prog
progAE = mkProg
  [ "x = a + b;"
  , "y = a * b;"
  , "while (y > a + b) {"
  , "  a = a + 1;"
  , "  x = a + b;"
  , "}"
  ]

reslAE :: [(Label, Set AExpr)]
reslAE = aexprs
  [ "{a + b}"
  , "{a * b, a + b}"
  , "{a + b}"
  , "{}"
  , "{a + b}"
  ]

aexprs :: [String] -> [(Label, Set AExpr)]
aexprs = zip [1..] . map (runParser "stdin" pAExprSet)
