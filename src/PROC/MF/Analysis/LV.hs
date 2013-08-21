module PROC.MF.Analysis.LV (mfLV) where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable
import PROC.MF.FreeNames

import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

-- * Monotone Framework Instance

-- |Monotone Framework for Live Variable Analysis.
mfLV :: Prog -> MF (Set Name)
mfLV p
  = backwards p
  $ distributive killLV genLV
  $ embelished p
  $ framework
  { getI = S.empty
  , getL = Lattice
    { join    = S.union
    , refines = flip S.isProperSubsetOf
    , bottom  = S.empty
    }
  }

killLV :: Stmt -> Set Name -> Set Name
killLV (Assign _ x _) _ = S.singleton x
killLV (Skip _)       _ = S.empty
killLV (BExpr _ b)    _ = S.empty
killLV (Call _ _ _ _) _ = S.empty

genLV :: Stmt -> Set Name
genLV (Assign _ _ a)  = freeNames a
genLV (Skip _)        = S.empty
genLV (BExpr _ b)     = freeNames b
genLV (Call _ _ _ as) = S.foldMap freeNames as
