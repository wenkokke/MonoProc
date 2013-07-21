module Main where

import PROC
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable (forM_)
import Text.Printf (printf)
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Idioms
import Text.ParserCombinators.UU.Utils (runParser)

prog :: Prog
prog = mkProg
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



main :: IO ()
main = forM_ resl $ \(l,exp) -> do
  let fnd = analyse mfLV prog l
  if exp == fnd
    then return ()
    else fail (printf "expected %s, found %s (at %d)" (show $ S.toList exp) (show $ S.toList fnd) l)
    