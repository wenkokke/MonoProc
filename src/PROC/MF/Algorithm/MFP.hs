{-# LANGUAGE TupleSections #-}
module PROC.MF.Algorithm.MFP where

import PROC.Base
import PROC.MF.Flowable
import PROC.MF.Analysis

import Control.Arrow (second)
import Control.Applicative ((<$>),(<|>))

import qualified Data.List as L

import Debug.Trace

import Data.Maybe (listToMaybe,fromJust,fromMaybe)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap,find)

import Text.Printf (printf)

-- * Maximal Fixed Point (MFP) Analysis.

mfp :: Algorithm a a
mfp mf l = mfp' mf l `runContext` []

-- |MFP computes the maximal fixpoint for an MF.
mfp' :: Algorithm a (Context CallStack a)
mfp' mf | isForwards  mf = mfpO mf
        | isBackwards mf = mfpI mf
  where
  mfpI mf l = fixMFP mf (mkWorkList mf) (mkAnalysis mf) l
  mfpO mf l = let tr = applyT' mf l in tr (mfpI mf l)
  
-- |A worklist is a list of flows still to examine.
type WorkList = [Flow]

-- |Compute the maximal fixed point for an MF.
fixMFP :: MF a -> WorkList -> Analysis (Context CallStack a) -> Analysis (Context CallStack a)
fixMFP mf [] mfp = mfp
fixMFP mf (Inter a b : ws) mfp
  | isEntry   = fixMFP mf ws' mfpC
  | otherwise = fixMFP mf ws' mfpR
  where
    -- 1: find the call/return statement for (a;b)
    call = (True ,) <$> findCall mf a
    retn = (False,) <$> findRetn mf b
    (isEntry,stmt) = fromJust $ call <|> retn
    (c,r,n,vals) = callToTuple stmt
    args = getArgs mf stmt
    
    -- 3: compute analysis for entry (used only when a == c and b == n)
    mfpC k = do
      let (c,n) = (a,b)    
      if k == c
        then applyT1' mf c (enter mf c args $ mfp c)
        else mfp k
    
    -- 4: compute analysis for exit (used only when a == x and b == r)
    mfpR k = do
      let (x,r) = (a,b)
      if k == r
        then applyT2' mf c (mfp c) (exit mf c args $ mfp x)
        else mfp k
    
    -- 5: compute new worklist
    ws' = filter (`flowsFrom` b) (mkWorkList mf) ++ ws

fixMFP mf (Intra a b : ws) mfp
  | mfpA <: mfpB = fixMFP mf ws' mfp'
  | otherwise    = fixMFP mf ws mfp
  where
    -- data: new analysis for l, old analysis for l'
    mfpA  = applyT' mf a (mfp a)
    mfpB  = mfp b
      
    -- recursive case: new worklist and intermediate result
    ws'    = filter (`flowsFrom` b) (mkWorkList mf) ++ ws
    mfp' k = if k == b then mfpA \/ mfpB else mfp k
    
    -- import: refines as (<:) and join as (\/)
    x <: y = refines (getL' mf) x y
    x \/ y = join (getL' mf) x y

-- |Initial worklist of the mfp algorithm.
mkWorkList :: MF a -> WorkList
mkWorkList mf = S.toList (getF mf)
  
-- |Initial output of the embelished Analysis algorithm.
mkAnalysis :: MF a -> Analysis (Context CallStack a)
mkAnalysis mf l
  | l `S.member` getE mf = getI' mf
  | otherwise            = bottom (getL' mf)

type CallStack = [Label]

data Context c a = Context
  { getContext :: [c]
  , runContext :: c -> a
  }
  
k_callstacks :: Int -> Prog -> [CallStack]
k_callstacks k p = k_sample k (calllabels p) where
  
  calllabels :: Prog -> [Label]
  calllabels = map (\(Call c _ _ _) -> c) . filter isCall . S.toList . blocks
    
  k_sample :: Int -> [a] -> [[a]]
  k_sample 0 _  = [[]]
  k_sample k xs = tr xs (k_sample (k - 1) xs) where
    tr :: [a] -> [[a]] -> [[a]]
    tr list acc = [ x : xs | x <- list, xs <- acc ]

instance (Show c, Show a) => Show (Context c a) where
  show c = "{\n"
        ++ (unlines $ map (\k -> printf ", %s -> %s" (show k) (show $ c `runContext` k)) (getContext c))
        ++ "}"
  
instance Functor (Context c) where
  fmap f c = c { runContext = f . runContext c }
  
-- |Lifts a lattice on a property space to a lattice on a
--  context-sensitive property space.
getL' :: MF a -> Lattice (Context CallStack a) -- or: (Eq c) => MF a -> Lattice (Context c a)
getL' mf = Lattice
  { join    = joinContext
  , refines = refinesContext
  , bottom  = bottomContext
  }
  where
  joinContext c1 c2 = Context
    { getContext = getContext c1 `L.union` getContext c2
    , runContext = \c -> join (getL mf) (c1 `runContext` c) (c2 `runContext` c)
    }
  refinesContext c1 c2 = L.any refinesAt allContexts
    where
    allContexts = getContext c1 `L.union` getContext c2
    refinesAt c = refines (getL mf) (c1 `runContext` c) (c2 `runContext` c)
  bottomContext = Context
    { getContext = []
    , runContext = const . bottom . getL $ mf
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
getT' mf s@(Call _ _ _ _) ca = ca
getT' mf s                ca = getT mf s <$> ca
  
-- |Applies the lifted transfer function that works pointwise.
applyT' :: MF a -> Label -> Context c a -> Context c a
applyT' mf l ca = applyT mf l <$> ca

-- |Transfer function for entering a procedure.
enter :: MF a -> Label -> [(Name,AExpr)] -> Context CallStack a -> Context CallStack a
enter mf c args ca = result
  where
  result    = foldr (<$>) ca (unassign : assigns)
  assigns   = map (getT mf . uncurry assign) args
  unassign  = getT mf $ assign "return" ANull

-- |Transfer function for leaving a procedure.
exit :: MF a -> Label -> [(Name,AExpr)] -> Context CallStack a -> Context CallStack a
exit mf c args ca = result
  where
  result    = foldr (<$>) ca unassigns
  unassigns = map (getT mf . flip assign ANull) (map fst args)

-- |Computes information upto a function call.
applyT1' :: MF a -> Label -> Context CallStack a -> Context CallStack a
applyT1' mf l c = c { runContext = runContext' }
  where
  runContext' cs
    | null cs || head cs /= l = bottom (getL mf)
    | otherwise               = c `runContext` (tail cs)
  
-- |Combines information at function returns.
applyT2' :: MF a -> Label -> Context CallStack a -> Context CallStack a -> Context CallStack a
applyT2' mf l c1 c2 = Context
  { getContext = getContext c1 `L.union` getContext c2
  , runContext = \cs -> runContext c1 cs \/ runContext c2 (l : cs)
  }
  where
  (\/) = join (getL mf)
  
-- |Find argument names and pair with argument values.
getArgs :: MF a -> Stmt -> [(Name,AExpr)]
getArgs mf (Call _ _ n vals) = case M.lookup n (getD mf) of
  Just (Decl _ names _) -> zip names vals
  Nothing              -> error ("undefined function \"" ++ show n ++ "\"")

-- |Finds a call statement with a specific call label.
findCall :: MF a -> Label -> Maybe Stmt
findCall mf c = listToMaybe filtered
  where
  blocks   = S.toList (getBlocks mf)
  filtered = filter isCall blocks
  isCall (Call c' _ _ _) = c == c'
  isCall _ = False

-- |Finds a call statement with a specific return label.  
findRetn :: MF a -> Label -> Maybe Stmt
findRetn mf r = listToMaybe filtered
  where
  blocks   = S.toList (getBlocks mf)
  filtered = filter isRetn blocks
  isRetn (Call _ r' _ _) = r == r'
  isRetn _ = False
  
-- |Converts a call statement into a tuple of values.
callToTuple :: Stmt -> (Label,Label,Name,[AExpr])
callToTuple (Call c r n vals) = (c,r,n,vals)
