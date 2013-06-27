module PROC.MF.Analysis.AE (mfAE) where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable

import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

mfAE :: Stmt -> MF (Set AExpr)
mfAE s
  = forwards s
  $ distributive killAE genAE
  $ framework
  { getI = S.empty
  , getL = Lattice
    { join    = S.intersection
    , refines = S.isSubsetOf
    , bottom  = available s
    }
  }

killAE :: Stmt -> Set AExpr -> Set AExpr
killAE (Assign _ x _) bot = S.filter (isFreeIn x) bot
killAE (Skip _)        _  = S.empty
killAE (BExpr _ _)     _  = S.empty

genAE :: Stmt -> Set AExpr
genAE (Assign _ x a) = S.filter (not . isFreeIn x) (available a)
genAE (Skip _)       = S.empty
genAE (BExpr _ _)    = S.empty
