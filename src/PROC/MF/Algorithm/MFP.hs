module PROC.MF.Algorithm.MFP (mfp) where

import PROC.Base
import PROC.MF.Flowable
import PROC.MF.Analysis

import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

-- * Maximal Fixed Point (MFP) Analysis.

-- |MFP computes the maximal fixpoint for an MF.
mfp :: Algorithm a
mfp mf | isForwards  mf = mfpO mf
       | isBackwards mf = mfpI mf
  where
  mfpI :: Algorithm a
  mfpI mf s = fixMFP mf s (mkWorkList mf) (mkAnalysis mf s)

  mfpO :: Algorithm a
  mfpO mf s l = let tr = getT mf s l in tr (mfpI mf s l)
  
-- |A worklist is a list of flows still to examine.
type WorkList = [Flow]

-- |Compute the maximal fixed point for an MF.
fixMFP :: MF a -> Stmt -> WorkList -> Analysis a -> Analysis a
fixMFP mf s [    ] mfp = mfp
fixMFP mf s (w:ws) mfp
  | mfpL <: mfpL' = fixMFP mf s ws' mfp'
  | otherwise     = fixMFP mf s ws mfp
  where
  
    -- data: new analysis for l, old analysis for l'
    mfpL   = tr (mfp l)
    mfpL'  = mfp l'
      
    -- recursive case: new worklist and intermediate result
    ws'    = filter (`flowsFrom` l') (mkWorkList mf) ++ ws
    mfp' k = if k == l' then mfpL \/ mfpL' else mfp k
    
    -- import: flow as (l,l') and transfer function as tr
    (l,l') = (from w, to w)
    tr     = getT mf s l 
    
    -- import: refines as (<:) and join as (\/)
    x <: y = refines (getL mf) x y
    x \/ y = join (getL mf) x y

-- |Initial worklist of the mfp algorithm.
mkWorkList :: MF a -> WorkList
mkWorkList mf = S.toList (getF mf)
      
-- |Initial output of the Analysis algorithm.
mkAnalysis :: MF a -> Stmt -> Analysis a
mkAnalysis mf s l
  | l `S.member` getE mf = getI mf
  | otherwise            = bottom (getL mf)
