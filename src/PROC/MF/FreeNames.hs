module PROC.MF.FreeNames where

import PROC.Base
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as S

-- * Free Variable Names

class FreeNames a where
  freeNames :: a -> Set Name
  isFreeIn :: Name -> a -> Bool
  isFreeIn x a = S.member x (freeNames a)
  
instance FreeNames Prog where
  freeNames (Prog _ s) = freeNames s

instance FreeNames Stmt where
  freeNames (Assign _ _ a)   = freeNames a
  freeNames (Skip _)         = S.empty
  freeNames (IfThen _ s1 s2) = freeNames s1 <> freeNames s2
  freeNames (While _ s1)     = freeNames s1
  freeNames (Seq s1 s2)      = freeNames s1 <> freeNames s2
  freeNames (Call _ _ _ _)   = S.empty
  
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
  