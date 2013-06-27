module PROC.MF.Analysis.RD where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable

import Text.Printf (printf)
import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

mfRD :: Stmt -> MF RD
mfRD s = MF
  { kill = killRD
  , gen  = genRD
  , getI = S.map (\x -> RD x Nothing) (freeNames s)
  , getE = S.singleton (init s)
  , getF = flow s
  , getL = Lattice
    { join    = S.union
    , refines = flip S.isProperSubsetOf
    , bottom  = S.empty
    }
  }

killRD :: Stmt -> Set RD -> Set RD
killRD (Assign _ x _) bot = S.insert (RD x Nothing) (S.filter (\(RD x' _) -> x == x') bot)
killRD (Skip _)        _  = S.empty
killRD (BExpr _ b)     _  = S.empty

genRD :: Stmt -> Set RD
genRD (Assign l x _)  = S.singleton (RD x (Just l))
genRD (Skip _)        = S.empty
genRD (BExpr _ _)     = S.empty

data RD = RD Name (Maybe Label)

instance Show RD where
  show (RD x Nothing ) = printf "{%s,?}" x
  show (RD x (Just l)) = printf "{%s,%d}" x l
  
instance Eq RD where
  (RD x _) == (RD y _) = x == y

instance Ord RD where
  compare (RD x _) (RD y _) = compare x y