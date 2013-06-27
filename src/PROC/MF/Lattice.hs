module PROC.MF.Lattice where

import PROC.Base
import Data.Set (Set,(\\))
import qualified Data.Set as S
  
data MF a = MF
  { kill :: Set a -> Stmt -> Set a
  , gen  :: Stmt -> Set a
  , getI :: Set a
  , getE :: Set Label
  , getF :: Set Flow
  , getL :: Lattice a
  }

data Lattice a = Lattice
  { join    :: Set a -> Set a -> Set a
  , refines :: Set a -> Set a -> Bool
  , bottom  :: Set a
  }
