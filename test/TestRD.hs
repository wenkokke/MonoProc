module Main where

import PROC
import PROC.MF.Analysis.RD
import PROC.Testing (testAnalysis)
import PROC.Parsing (pRDSet)

import Data.Set (Set)
import Text.ParserCombinators.UU.Utils (runParser)

main :: IO ()
main = do testAnalysis mfp mfRD progRD reslRD
          testAnalysis (mopk 3) mfRD progRD reslRD

progRD :: Prog
progRD = mkProg
  [ "x = 5;"
  , "y = 1;"
  , "while (x > 1) {"
  , "  y = x * y;"
  , "  x = x - 1;"
  , "}"
  ]

reslRD :: [(Label, Set RD)]
reslRD = rds
  [ "{(y,?), (x,1)}"
  , "{(x,1), (y,2)}"
  , "{(x,1), (y,2), (y,4), (x,5)}"
  , "{(x,1), (y,4), (x,5)}"
  , "{(x,1), (y,4)}"
  ]

rds :: [String] -> [(Label,Set RD)]
rds = zip [1..] . map (runParser "stdin" pRDSet)
