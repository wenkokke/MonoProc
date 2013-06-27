{-# LANGUAGE ExistentialQuantification #-}
module PROC.MF.Analysis where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Flowable
import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

-- |Analysis at the input label.
analyseI :: (Ord a) => MF a -> Stmt -> Label -> a
analyseI mf s l
  | S.member l (getE mf) = (getI mf)
  | otherwise = let
    fromSet   = S.map from (S.filter ((l==) . to) (getF mf))
    outSets   = S.map (analyseO mf s) fromSet
    in S.fold (join $ getL mf) (bottom $ getL mf) outSets

-- |Analysis at the output label.
analyseO :: (Ord a) => MF a -> Stmt -> Label -> a
analyseO mf s l = getT mf s l (analyseI mf s l)

-- * Monotone Frameworks
  
-- |Type for @kill@ functions of MF's.
type Transfer a = Stmt -> Label -> a -> a

data MF a = MF
  { getI :: a          -- ^ extremal values
  , getE :: Set Label  -- ^ extremal labels
  , getF :: Set Flow   -- ^ control flow for analysis
  , getL :: Lattice a  -- ^ lattice on property space
  , getT :: Transfer a -- ^ transfer function
  }

data Lattice a = Lattice
  { join    :: a -> a -> a
  , refines :: a -> a -> Bool
  , bottom  :: a
  }

-- * MF transformers

-- |Easily make MF's for backwards analyses.
forwards :: Stmt -> MF a -> MF a
forwards s mf = mf
  { getE = S.singleton (init s)
  , getF = flow s
  }

-- |Easily make MF's for forwards analyses.
backwards :: Stmt -> MF a -> MF a
backwards s mf = mf
  { getE = final s
  , getF = flowR s
  }
  
-- |Type for @kill@ functions of distributive MF's.
type Kill a = Stmt -> a -> a

-- |Type for @gen@ functions of distributive MF's.
type Gen a = Stmt -> a
  
-- |Easily make distributive monotone frameworks.
distributive :: (Ord a) => Kill (Set a) -> Gen (Set a) -> MF (Set a) -> MF (Set a)
distributive kill gen mf = mf { getT = transfer }
  where
  transfer s l rs = (rs \\ killed) <> genned
    where
    block  = select l (blocks s)
    killed = kill block (bottom $ getL mf)
    genned = gen block

-- |Empty monotone framework.
framework :: MF a
framework = MF
  { getI = undefined
  , getE = undefined
  , getF = undefined
  , getL = undefined
  , getT = undefined
  }
  
-- * Free variable name analysis

class FreeNames a where
  freeNames :: a -> Set Name
  isFreeIn :: Name -> a -> Bool
  isFreeIn x a = S.member x (freeNames a)

instance FreeNames Stmt where
  freeNames (Assign _ _ a)   = freeNames a
  freeNames (Skip _)         = S.empty
  freeNames (IfThen _ s1 s2) = freeNames s1 <> freeNames s2
  freeNames (While _ s1)     = freeNames s1
  freeNames (Call _ _ _)     = S.empty
  freeNames (Seq s1 s2)      = freeNames s1 <> freeNames s2
  
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

-- * Available expression analysis
--   Not to be confused with the AE monotone analysis

class Available a where
  available :: a -> Set AExpr

instance Available Stmt where
  available = S.foldMap available' . blocks
    where
    available' (Assign _ _ a) = available a
    available' (BExpr _ b)    = available b
    available' _ = S.empty

instance Available BExpr where
  available (BConst _)  = S.empty
  available (Lt a1 a2)  = available a1 <> available a2
  available (Lte a1 a2) = available a1 <> available a2
  available (Gt a1 a2)  = available a1 <> available a2
  available (Gte a1 a2) = available a1 <> available a2
  available (Eq a1 a2)  = available a1 <> available a2
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
  