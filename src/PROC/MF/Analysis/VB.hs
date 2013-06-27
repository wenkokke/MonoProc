module PROC.MF.Analysis.VB where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable

import Text.Printf (printf)
import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

mfVB :: Stmt -> MF (Set AExpr)
mfVB s
  = backwards s
  $ distributive killVB genVB
  $ framework
  { getI = S.empty
  , getE = final s
  , getF = flowR s
  , getL = Lattice
    { join    = S.intersection
    , refines = S.isSubsetOf
    , bottom  = available s
    }
  }

killVB :: Stmt -> Set AExpr -> Set AExpr
killVB (Assign _ x _) bot = S.filter (isFreeIn x) bot
killVB (Skip _)        _  = S.empty
killVB (BExpr _ b)     _  = S.empty

genVB :: Stmt -> Set AExpr
genVB (Assign _ _ a) = available a
genVB (Skip _)       = S.empty
genVB (BExpr _ b)    = available b
