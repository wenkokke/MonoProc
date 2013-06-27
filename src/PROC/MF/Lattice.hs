module PROC.MF.Lattice where

import PROC.Base
import Data.Set (Set,(\\))
import qualified Data.Set as S

class Lattice a where
  extremalValues :: Set a
--extremalLabels :: Set Label
