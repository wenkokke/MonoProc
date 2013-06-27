module PROC.MF.Analysis.AE where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable

import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

class Available a where
  available :: a -> Set AExpr

instance Available Stmt where
  available = S.foldMap available' . blocks
    where
    available' (Assign _ _ a) = available a
    available' (BExpr _ b)    = available b
    available' _ = S.empty

instance Available BExpr where
  available (BConst _)  = S.empty
  available (Lt a1 a2)  = available a1 <> available a2
  available (Lte a1 a2) = available a1 <> available a2
  available (Gt a1 a2)  = available a1 <> available a2
  available (Gte a1 a2) = available a1 <> available a2
  available (Eq a1 a2)  = available a1 <> available a2
  available (Neq a1 a2) = available a1 <> available a2
  available (Not a1)    = available a1
    
instance Available AExpr where
  available (AName _)     = S.empty
  available (AConst _)    = S.empty
  available a@(Add e1 e2) = S.insert a (available e1 <> available e1)
  available a@(Sub e1 e2) = S.insert a (available e1 <> available e1)
  available a@(Mul e1 e2) = S.insert a (available e1 <> available e1)
  available a@(Div e1 e2) = S.insert a (available e1 <> available e1)
  available a@(Neg e1)    = S.insert a (available e1)

killAE :: Set AExpr -> Stmt -> Set AExpr
killAE ae (Assign _ x a) = S.filter (isFreeIn x) ae
killAE ae (Skip _)       = S.empty
killAE ae (BExpr _ _)    = S.empty

genAE :: Stmt -> Set AExpr
genAE (Assign _ x a) = S.filter (not . isFreeIn x) (available a)
genAE (Skip _)       = S.empty
genAE (BExpr _ _)    = S.empty

entryAE :: Stmt -> Label -> Set AExpr
entryAE s l
  | l == init s = S.empty
  | otherwise   = S.fold S.intersection (iotaAE s)
                  (S.map (exitAE s . from) (S.filter ((l ==) . to) (flow s)))

exitAE :: Stmt -> Label -> Set AExpr
exitAE s l = let
  b = select l (blocks s)
  k = killAE (iotaAE s) b
  g = genAE b
  in (entryAE s l \\ k) <> g
  
iotaAE :: Stmt -> Set AExpr
iotaAE = available

  