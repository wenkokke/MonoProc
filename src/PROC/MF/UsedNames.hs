module PROC.MF.UsedNames where

import PROC.Base
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as S

-- * Assigned Variable Names

class UsedNames a where
  usedNames :: a -> Set Name
  isUsedIn :: Name -> a -> Bool
  isUsedIn x a = S.member x (usedNames a)

instance UsedNames Prog where
  usedNames (Prog _ s) = usedNames s
  
instance UsedNames Stmt where
  usedNames (Assign _ x _)   = S.singleton x
  usedNames (Skip _)         = S.empty
  usedNames (IfThen _ s1 s2) = usedNames s1 <> usedNames s2
  usedNames (While _ s1)     = usedNames s1
  usedNames (Seq s1 s2)      = usedNames s1 <> usedNames s2
  usedNames (Call _ _ _ _)   = S.empty
