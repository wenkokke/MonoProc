module PROC.MF.Available where

import PROC.Base
import PROC.MF.Flowable (blocks)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

-- * Available Expressions

class Available a where
  available :: a -> Set AExpr
  
instance (Available a) => (Available [a]) where
  available = foldr (<>) S.empty . map available
  
instance Available Prog where
  available (Prog d s) = available d <> available s
  
instance Available Decl where
  available (Decl _ _ s) = available s

instance Available Stmt where
  available = S.foldMap available' . blocks
    where
    available' (Assign _ _ a)  = available a
    available' (BExpr _ b)     = available b
    available' (Call _ _ _ as) = available as
    available' _               = S.empty

instance Available BExpr where
  available (BConst _)  = S.empty
  available (Lt  a1 a2) = available a1 <> available a2
  available (Lte a1 a2) = available a1 <> available a2
  available (Gt  a1 a2) = available a1 <> available a2
  available (Gte a1 a2) = available a1 <> available a2
  available (Eq  a1 a2) = available a1 <> available a2
  available (Neq a1 a2) = available a1 <> available a2
  available (Not a1)    = available a1
    
instance Available AExpr where
  available (AName _)     = S.empty
  available (AConst _)    = S.empty
  available a@(Add e1 e2) = S.insert a (available e1 <> available e1)
  available a@(Sub e1 e2) = S.insert a (available e1 <> available e1)
  available a@(Mul e1 e2) = S.insert a (available e1 <> available e1)
  available a@(Div e1 e2) = S.insert a (available e1 <> available e1)
  available a@(Neg e1)    = S.insert a (available e1)