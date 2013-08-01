module Main where

import PROC
import PROC.MF.Analysis.LV
import PROC.Testing (testAnalysis)
import PROC.Parsing (pNameSet)

import Data.Set (Set)
import Text.ParserCombinators.UU.Utils (runParser)

main :: IO ()
main = do testAnalysis mfp mfLV progLV reslLV
          testAnalysis mop mfLV progLV reslLV

progLV :: Prog
progLV = mkProg
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

reslLV :: [(Label, Set Name)]
reslLV = vars
  [ "{}"
  , "{y}"
  , "{x,y}"
  , "{y}"
  , "{z}"
  , "{z}"
  , "{}"
  ]
  
vars :: [String] -> [(Label, Set Name)]
vars = zip [1..] . map (runParser "stdin" pNameSet)
