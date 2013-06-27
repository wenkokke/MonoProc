module PROC.MF.Analysis.LV where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable

import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

killLV :: Set Name -> Stmt -> Set Name
killLV _ (Assign _ x _) = S.singleton x
killLV _ (Skip _)       = S.empty
killLV _ (BExpr _ b)    = S.empty

genLV :: Stmt -> Set Name
genLV (Assign _ _ a)  = freeNames a
genLV (Skip _)        = S.empty
genLV (BExpr _ b)     = freeNames b

exitLV :: Stmt -> Label -> Set Name
exitLV s l
  | S.member l (final s) = S.empty
  | otherwise = S.fold S.union (iotaLV s)
                (S.map (entryLV s . from) (S.filter ((l ==) . to) (flowR s)))
  
entryLV :: Stmt -> Label -> Set Name
entryLV s l = let
  b = select l (blocks s)
  k = killLV (iotaLV s) b
  g = genLV b
  in (exitLV s l \\ k) <> g

iotaLV :: Stmt -> Set Name
iotaLV _ = S.empty