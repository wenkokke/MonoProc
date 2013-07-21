{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import PROC
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable (forM_)
import Text.Printf (printf)

import Text.ParserCombinators.UU ((<$),(<$>),(<|>),pListSep)
import Text.ParserCombinators.UU.BasicInstances (Parser,pSym)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.Utils (runParser,pComma,pNatural,pBraces)

main :: IO ()
main = forM_ resl $ \(l,exp) -> do
  let fnd = analyse mfLV prog l
  if exp == fnd
    then return ()
    else fail (printf "expected %s, found %s (at %d)" (show $ S.toList exp) (show $ S.toList fnd) l)

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

resl :: [(Label, Set Name)]
resl = vars
  [ "{}"
  , "{y}"
  , "{x,y}"
  , "{y}"
  , "{z}"
  , "{z}"
  , "{}"
  ]

vars :: [String] -> [(Label, Set Name)]
vars = zip [1..] . map (runParser "stdin" pVars)
  
pVars :: Parser (Set Name)
pVars = S.fromList <$> pBraces (pListSep pComma pName)
