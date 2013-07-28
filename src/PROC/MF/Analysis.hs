{-# LANGUAGE ExistentialQuantification #-}
module PROC.MF.Analysis where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Flowable
import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

analyseAndShow :: (Show a) => (Prog -> MF a) -> Prog -> String
analyseAndShow mf p = showMFP (S.toList $ labels p) (analyse mf p)

-- |Runs an MFP analysis on a program at a certain label.
analyse :: (Prog -> MF a) -> Prog -> Label -> a
analyse mkMF p@(Prog d s) l
  | S.member l (labels p) = let
       mf  = mkMF p
       mfp = runMFP mf mf s
    in mfp l
  | otherwise             = error ("no statement with label " ++ show l)

analyseI :: MF a -> Stmt -> MFP a
analyseI mf s = fixMFP mf s (ws0 mf) (mfp0 mf s)

analyseO :: MF a -> Stmt -> MFP a
analyseO mf s l = let tr = getT mf s l in tr (analyseI mf s l)

-- * Maximal Fixed Point Analysis

type MFP a = Label -> a

-- |Compute the maximal fixed point for an MF.
fixMFP :: MF a -> Stmt -> [Flow] -> MFP a -> MFP a
fixMFP mf s [    ] mfp = mfp
fixMFP mf s (w:ws) mfp = let

    (l, l') = (from w, to w)        -- import flow as (l,l')
    tr      = getT mf s l           -- import transfer function t
    
    x <: y = refines (getL mf) x y  -- import @refines@ operator
    x \/ y = join (getL mf) x y     -- import @join@ operator
    
    mfpL    = tr (mfp l)            -- get new analysis for l
    mfpL'   = mfp l'                -- get old analysis for l'

  in if not (mfpL <: mfpL')         -- if we have a fixpoint,
      then fixMFP mf s ws mfp       -- stop and return the current analysis
      else let                      -- otherwise,
      
        -- add all flows from l' to the worklist
        ws'    = filter (`flowsFrom` l') (ws0 mf) ++ ws
        -- add the join of both computations to the result
        mfp' k = if k == l' then mfpL \/ mfpL' else mfp k
        
      in fixMFP mf s ws' mfp'       -- recusively perform the analysis
      
-- |Show instances of MFP--which are functions--for a limited
--  number of inputs.
showMFP :: (Show a) => [Label] -> MFP a -> String
showMFP ls mfp = unlines $ map (show . mfp) ls

-- |Initial worklist of the mfp algorithm.
ws0 :: MF a -> [Flow]
ws0 mf = S.toList (getF mf)
      
-- |Initial output of the MFP algorithm.
mfp0 :: MF a -> Stmt -> MFP a
mfp0 mf s l
  | l `S.member` getE mf = getI mf
  | otherwise            = bottom (getL mf)

-- * Monotone Frameworks
  
-- |Type for transfer functions of MF's.
type Transfer a = Stmt -> Label -> a -> a

data MF a = MF
  { getI   :: a                     -- ^ extremal values
  , getE   :: Set Label             -- ^ extremal labels
  , getF   :: Set Flow              -- ^ control flow for analysis
  , getL   :: Lattice a             -- ^ lattice on property space
  , getT   :: Transfer a            -- ^ transfer function
  , getD   :: Env                   -- ^ procedure declarations
  , runMFP :: MF a -> Stmt -> MFP a -- ^ run the analysis
  }
  
-- * Lattices

data Lattice a = Lattice
  { join    :: a -> a -> a
  , refines :: a -> a -> Bool
  , bottom  :: a
  }
  
joinall :: Lattice a -> [a] -> a
joinall l = foldr (join l) (bottom l)

-- * MF transformers

-- |Easily make MF's for backwards analyses.
forwards :: Stmt -> MF a -> MF a
forwards s mf = mf
  { getE   = S.singleton (init s)
  , getF   = flow (getD mf) s
  , runMFP = analyseO
  }

-- |Easily make MF's for forwards analyses.
backwards :: Stmt -> MF a -> MF a
backwards s mf = mf
  { getE   = final s
  , getF   = flowR (getD mf) s
  , runMFP = analyseI
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

-- |Easily make embellished monotone frameworks.
embelished :: Env -> MF a -> MF a
embelished env mf = mf { getD = env }
    
-- |Empty monotone framework.
framework :: MF a
framework  = MF
  { getI   = error "uninitialized property I"
  , getE   = error "uninitialized property E"
  , getF   = error "uninitialized property F"
  , getL   = error "uninitialized property L"
  , getT   = error "uninitialized property T"
  , getD   = error "uninitialized property D"
  , runMFP = error "uninitialized function `run'"
  }
  
-- * Free Variable Names

class FreeNames a where
  freeNames :: a -> Set Name
  isFreeIn :: Name -> a -> Bool
  isFreeIn x a = S.member x (freeNames a)

instance FreeNames Stmt where
  freeNames (Assign _ _ a)   = freeNames a
  freeNames (Skip _)         = S.empty
  freeNames (IfThen _ s1 s2) = freeNames s1 <> freeNames s2
  freeNames (While _ s1)     = freeNames s1
  freeNames (Call _ _ _ _)   = S.empty
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

-- * Available Expressions

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
  