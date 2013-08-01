module Main where

import PROC
import PROC.MF.Analysis.VB
import PROC.Testing (testAnalysis)
import PROC.Parsing (pAExprSet)

import Data.Set (Set)
import Text.ParserCombinators.UU.Utils (runParser)

main :: IO ()
main = do testAnalysis mfp mfVB progVB reslVB
          testAnalysis mop mfVB progVB reslVB
    
progVB :: Prog
progVB = mkProg
  [ "if (a > b) {"
  , "  x = b - a;"
  , "  y = a - b;"
  , "}"
  , "else {"
  , "  y = b - a;"
  , "  x = a - b;"
  , "}"
  ]

reslVB :: [(Label, Set AExpr)]
reslVB = aexprs
  [ "{a - b, b - a}"
  , "{a - b}"
  , "{}"
  , "{a - b}"
  , "{}"
  ]

aexprs :: [String] -> [(Label, Set AExpr)]
aexprs = zip [1..] . map (runParser "stdin" pAExprSet)