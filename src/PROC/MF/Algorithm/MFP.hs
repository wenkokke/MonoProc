module PROC.MF.Algorithm.MFP where

import PROC.Base
import PROC.MF.Flowable
import PROC.MF.Analysis

import Control.Applicative ((<$>),(<|>))

import qualified Data.List as L

import Data.Maybe (fromMaybe)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap,find)

import Text.Printf (printf)

-- * Maximal Fixed Point (MFP) Analysis.

mfp :: Algorithm a a
mfp mf s l = mfp' mf s l `runContext` []

-- |MFP computes the maximal fixpoint for an MF.
mfp' :: Algorithm a (Context CallStack a)
mfp' mf | isForwards  mf = mfpO mf
        | isBackwards mf = mfpI mf
  where
  mfpI mf s l = fixMFP mf s (mkWorkList  mf) (mkAnalysis mf s) l
  mfpO mf s l = let tr = applyT' mf s l in tr (mfpI mf s l)
  
-- |A worklist is a list of flows still to examine.
type WorkList = [Flow]

-- |Compute the maximal fixed point for an MF.
fixMFP :: MF a -> Stmt -> WorkList -> Analysis (Context CallStack a) -> Analysis (Context CallStack a)
fixMFP mf s [] mfp = mfp
fixMFP mf s (Inter a b : ws) mfp = fixMFP mf s ws mfp
fixMFP mf s (Intra a b : ws) mfp
  | mfpA <: mfpB = fixMFP mf s ws' mfp'
  | otherwise    = fixMFP mf s ws mfp
  where
    -- data: new analysis for l, old analysis for l'
    mfpA  = applyT' mf s a (mfp a)
    mfpB  = mfp b
      
    -- recursive case: new worklist and intermediate result
    ws'    = filter (`flowsFrom` b) (mkWorkList mf) ++ ws
    mfp' k = if k == b then mfpA \/ mfpB else mfp k
    
    -- import: refines as (<:) and join as (\/)
    x <: y = refines (getL' mf) x y
    x \/ y = join (getL' mf) x y
  
findCallOrRetn :: Label -> Label -> Set InterFlow -> InterFlow
findCallOrRetn c1 r1 fs =
  undefined
  
findCall :: Label -> Set InterFlow -> Maybe InterFlow
findCall c1 = S.find (\(c2,_,_,_) -> c1 == c2)

findRetn :: Label -> Set InterFlow -> Maybe InterFlow
findRetn r1 = S.find (\(_,_,_,r2) -> r1 == r2)

-- |Initial worklist of the mfp algorithm.
mkWorkList :: MF a -> WorkList
mkWorkList mf = S.toList (getF mf)
  
-- |Initial output of the embelished Analysis algorithm.
mkAnalysis :: MF a -> Stmt -> Analysis (Context CallStack a)
mkAnalysis mf s l
  | l `S.member` getE mf = getI' mf
  | otherwise            = bottom (getL' mf)

type CallStack = [Label]

data Context c a = Context
  { getContext :: [c]
  , runContext :: c -> a
  }
  
instance (Show c, Show a) => Show (Context c a) where
  show c = unlines $ map (\k -> printf "[%s] -> %s" (show k) (show $ runContext c k)) (getContext c)
  
instance Functor (Context c) where
  fmap f c = c { runContext = f . runContext c }
  
-- |Lifts a lattice on a property space to a lattice on a
--  context-sensitive property space.
getL' :: (Eq c) => MF a -> Lattice (Context c a)
getL' mf = Lattice
  { join    = joinContext
  , refines = refinesContext
  , bottom  = bottomContext
  }
  where
  joinContext c1 c2 = Context
    { getContext = getContext c1 `L.union` getContext c2
    , runContext  = \c -> join (getL mf) (c1 `runContext` c) (c2 `runContext` c)
    }
  refinesContext c1 c2 = L.any refinesAt allContexts
    where
    allContexts = getContext c1 `L.union` getContext c2
    refinesAt c = refines (getL mf) (c1 `runContext` c) (c2 `runContext` c)
  bottomContext = Context
    { getContext = []
    , runContext  = const . bottom . getL $ mf
    }

-- |Computes the extremal value of an MF based on the callstack
--  in the context.
getI' :: MF a -> Context CallStack a
getI' mf = Context
  { getContext = [[]]
  , runContext = \c -> case c of
      [] -> getI mf
      cs -> bottom (getL mf)
  }
  
-- |Computes the lifted transfer function that works pointwise.
getT' :: MF a -> Transfer (Context CallStack a)
getT' mf s ca = getT mf s <$> ca
  
-- |Applies the lifted transfer function that works pointwise.
applyT' :: MF a -> Stmt -> Label -> Context c a -> Context c a
applyT' mf s l ca = applyT mf s l <$> ca

-- |Computes information upto a function call.
getT1' :: MF a -> Label -> Context CallStack a -> Context CallStack a
getT1' mf l c = c { runContext = defn }
  where
  defn cs
    |  null cs || head cs /= l  = bottom (getL mf)
    |  otherwise                = c `runContext` cs
  
-- |Combines information at function returns.
getT2' :: MF a -> Label -> Context CallStack a -> Context CallStack a -> Context CallStack a
getT2' mf l c1 c2 = Context
  { getContext = getContext c1 `L.union` getContext c2
  , runContext = \cs -> runContext c1 cs \/ runContext c2 (l : cs)
  }
  where
  (\/) = join (getL mf)

