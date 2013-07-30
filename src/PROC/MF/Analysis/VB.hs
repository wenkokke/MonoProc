module PROC.MF.Analysis.VB (mfVB) where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable
import PROC.MF.Available
import PROC.MF.FreeNames

import Text.Printf (printf)
import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

mfVB :: Prog -> MF (Set AExpr)
mfVB p
  = backwards p
  $ embelished p
  $ distributive killVB genVB
  $ framework
  { getI = S.empty
  , getL = Lattice
    { join    = S.intersection
    , refines = S.isProperSubsetOf
    , bottom  = available p
    }
  }

killVB :: Stmt -> Set AExpr -> Set AExpr
killVB (Assign _ x _) bot = S.filter (x `isFreeIn`) bot
killVB (Skip _)        _  = S.empty
killVB (BExpr _ _)     _  = S.empty

genVB :: Stmt -> Set AExpr
genVB (Assign _ _ a) = available a
genVB (Skip _)       = S.empty
genVB (BExpr _ b)    = available b
