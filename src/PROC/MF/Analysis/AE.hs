module PROC.MF.Analysis.AE (mfAE) where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable
import PROC.MF.Available
import PROC.MF.FreeNames

import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

-- * Monotone Framework Instance

-- |Monotone Framework for Available Expression Analysis.
mfAE :: Prog -> MF (Set AExpr)
mfAE p
  = forwards p
  $ embelished p
  $ distributive killAE genAE
  $ framework
  { getI = S.empty
  , getL = Lattice
    { join    = S.intersection
    , refines = S.isProperSubsetOf
    , bottom  = available p
    }
  }

killAE :: Stmt -> Set AExpr -> Set AExpr
killAE (Assign _ x _) bot = S.filter (isFreeIn x) bot
killAE (Skip _)        _  = S.empty
killAE (BExpr _ _)     _  = S.empty

genAE :: Stmt -> Set AExpr
genAE (Assign _ x a) = S.filter (not . isFreeIn x) (available a)
genAE (Skip _)       = S.empty
genAE (BExpr _ b)    = S.empty
