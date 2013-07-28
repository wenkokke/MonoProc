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
  let fnd = analyse mfRD prog l
  if exp == fnd
    then return ()
    else fail (printf "expected %s, found %s (at %d)" (show $ S.toList exp) (show $ S.toList fnd) l)

prog :: Prog
prog = mkProg
  [ "x = 5;"
  , "y = 1;"
  , "while (x > 1) {"
  , "  y = x * y;"
  , "  x = x - 1;"
  , "}"
  ]

resl :: [(Label, Set RD)]
resl = rds
  [ "{(y,?), (x,1)}"
  , "{(x,1), (y,2)}"
  , "{(x,1), (y,2), (y,4), (x,5)}"
  , "{(x,1), (y,4), (x,5)}"
  , "{(x,1), (y,4)}"
  ]

rds :: [String] -> [(Label,Set RD)]
rds = zip [1..] . map (runParser "stdin" pRDs)
  
pRDs :: Parser (Set RD)
pRDs = S.fromList <$> pBraces (pListSep pComma pRD)
  
pRD :: Parser RD
pRD = iI RD "(" pName "," (pNone <|> pSome) ")" Ii
  where
  pNone = Nothing <$ pSym '?'
  pSome = Just <$> pNatural
