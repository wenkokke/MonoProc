module PROC.MF.Analysis.CP where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable

import Text.Printf (printf)
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative (pure,(<$>),(<*>))

-- incorrect, we need both TOP and BOTTOM
type CP = Map Name Integer

mfCP :: Stmt -> MF CP
mfCP s
  = forwards s
  $ framework
  { getI = M.empty
  , getL = Lattice
    { join    = M.union
    , refines = undefined
    , bottom  = M.empty
    }
  , getT = transferCP
  }
  
-- incorrect, function is not total but needs to be
transferCP :: Stmt -> Label -> CP -> CP
transferCP (Assign _ x a) _ m = maybe m (\i -> M.insert x i m) (evalAE m a)
transferCP (Skip _)       _ m = m
transferCP (BExpr _ _)    _ m = m

evalAE :: CP -> AExpr -> Maybe Integer
evalAE env a = case a of
  AName n   -> M.lookup n env
  AConst i  -> pure i
  Add e1 e2 -> (+) <$> evalAE env e1 <*> evalAE env e2
  Sub e1 e2 -> (-) <$> evalAE env e1 <*> evalAE env e2
  Mul e1 e2 -> (*) <$> evalAE env e1 <*> evalAE env e2
  Div e1 e2 -> div <$> evalAE env e1 <*> evalAE env e2
  Neg e1    -> negate <$> evalAE env e1
