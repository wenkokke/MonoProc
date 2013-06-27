module PROC.MF.Analysis where

import PROC.Base
import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

class FreeNames a where
  freeNames :: a -> Set Name
  isFreeIn :: Name -> a -> Bool
  isFreeIn x a = S.member x (freeNames a)

instance FreeNames BExpr where
  freeNames (BConst _)  = S.empty
  freeNames (Lt a1 a2)  = freeNames a1 <> freeNames a2
  freeNames (Lte a1 a2) = freeNames a1 <> freeNames a2
  freeNames (Gt a1 a2)  = freeNames a1 <> freeNames a2
  freeNames (Gte a1 a2) = freeNames a1 <> freeNames a2
  freeNames (Eq a1 a2)  = freeNames a1 <> freeNames a2
  freeNames (Neq a1 a2) = freeNames a1 <> freeNames a2
  freeNames (Not a1)    = freeNames a1
  
instance FreeNames AExpr where
  freeNames (AName n)   = S.singleton n
  freeNames (AConst _)  = S.empty
  freeNames (Add e1 e2) = freeNames e1 <> freeNames e2
  freeNames (Sub e1 e2) = freeNames e1 <> freeNames e2
  freeNames (Mul e1 e2) = freeNames e1 <> freeNames e2
  freeNames (Div e1 e2) = freeNames e1 <> freeNames e2
  freeNames (Neg e1)    = freeNames e1