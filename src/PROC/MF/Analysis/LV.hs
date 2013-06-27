module PROC.MF.Analysis.LV where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable

import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

mfLV :: Prog -> MF (Set Name)
mfLV (Prog d s)
  = backwards s
  $ distributive killLV genLV
  $ embelished (toEnv d)
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

genLV :: Stmt -> Set Name
genLV (Assign _ _ a)  = freeNames a
genLV (Skip _)        = S.empty
genLV (BExpr _ b)     = freeNames b
