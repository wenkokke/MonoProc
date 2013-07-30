{-# LANGUAGE ExistentialQuantification #-}
module PROC.MF.Analysis where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Flowable
import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

-- |Runs an MFP analysis on a program and shows the result.
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
fixMFP mf s (w:ws) mfp
  | mfpL <: mfpL' = fixMFP mf s ws' mfp'
  | otherwise     = fixMFP mf s ws mfp
  where
  
    -- data: new analysis for l, old analysis for l'
    mfpL   = tr (mfp l)
    mfpL'  = mfp l'
      
    -- recursive case: new worklist and intermediate result
    ws'    = filter (`flowsFrom` l') (ws0 mf) ++ ws
    mfp' k = if k == l' then mfpL \/ mfpL' else mfp k
    
    -- import: flow as (l,l') and transfer function as tr
    (l,l') = (from w, to w)
    tr     = getT mf s l
    
    -- import: refines as (<:) and join as (\/)
    x <: y = refines (getL mf) x y
    x \/ y = join (getL mf) x y
      
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
  , getIF  :: Set InterFlow         -- ^ inter-procedural flow
  , getL   :: Lattice a             -- ^ lattice on property space
  , getT   :: Transfer a            -- ^ transfer function
  , getD   :: FTable                   -- ^ procedure declarations
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
forwards :: Prog -> MF a -> MF a
forwards p@(Prog _ s) mf = mf
  { getE   = S.singleton (init s)
  , getF   = flow (getD mf) s
  , getIF  = interFlow (getD mf) p
  , runMFP = analyseO
  }

-- |Easily make MF's for forwards analyses.
backwards :: Prog -> MF a -> MF a
backwards p@(Prog _ s) mf = mf
  { getE   = final s
  , getF   = flowR (getD mf) s
  , getIF  = interFlowR (getD mf) p
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
    
-- |Context for embelished monotone frameworks.
data Context = Context

-- |Easily make embellished monotone frameworks.
embelished :: Prog -> MF a -> MF a
embelished (Prog d _) mf = mf { getD = mkFTable d }
    
-- |Empty monotone framework.
framework :: MF a
framework  = MF
  { getI   = error "uninitialized property I"
  , getE   = error "uninitialized property E (apply 'backwards' or 'forwards')"
  , getF   = error "uninitialized property F (apply 'backwards' or 'forwards')"
  , getIF  = error "uninitialized property IF (apply 'backwards' or 'forwards')"
  , getL   = error "uninitialized property L"
  , getT   = error "uninitialized property T"
  , getD   = error "uninitialized property D (apply 'embelished' first)"
  , runMFP = error "uninitialized function `run'"
  }
  