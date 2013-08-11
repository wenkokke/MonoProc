module PROC.Testing where

import PROC

import Data.Foldable (forM_)
import Text.Printf (printf)

testAnalysis :: (Show b, Eq b) => Algorithm a b -> (Prog -> MF a) -> Prog -> [(Label, b)] -> IO ()
testAnalysis alg mf prog resl = forM_ resl $ \(l,exp) -> do
  let fnd = analyse alg mf prog l
  if exp == fnd
    then return ()
    else fail (printf "expected %s, found %s (at %d)" (show exp) (show fnd) l)
