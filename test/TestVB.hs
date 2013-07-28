{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import PROC
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable (forM_)
import Text.Printf (printf)
import Text.ParserCombinators.UU ((<$>),pListSep)
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Utils (runParser,pBraces,pComma)

main :: IO ()
main = forM_ resl $ \(l,exp) -> do
  let fnd = analyse mfAE prog l
  if exp == fnd
    then return ()
    else fail (printf "expected %s, found %s (at %d)" (show $ S.toList exp) (show $ S.toList fnd) l)
    
prog :: Prog
prog = mkProg
  [ "if (a > b) {"
  , "  x = b - a;"
  , "  y = a - b;"
  , "}"
  , "else {"
  , "  y = b - a;"
  , "  x = a - b;"
  , "}"
  ]

resl :: [(Label, Set AExpr)]
resl = aexprs
  [ "{a - b, b - a}"
  , "{a - b}"
  , "{}"
  , "{a - b}"
  , "{}"
  ]

aexprs :: [String] -> [(Label, Set AExpr)]
aexprs = zip [1..] . map (runParser "stdin" pAExprSet)
  
pAExprSet :: Parser (Set AExpr)
pAExprSet = S.fromList <$> pBraces (pListSep pComma pAExpr)