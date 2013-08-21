{-# LANGUAGE TupleSections #-}
module PROC.MF.Algorithm.MFP (mfp,mfpk,mfp',mfpk') where

import PROC.Base
import PROC.MF.Flowable
import PROC.MF.Analysis

import Control.Applicative ((<$>),(<|>))

import Data.Maybe (listToMaybe)
import qualified Data.List as L
import qualified Data.Set as S

import Text.Printf (printf)

-- * Maximal Fixed-Point (MFP) Analysis

-- |Performs pointwise maximal fixed-point analysis (as by @mfp'@), using call
--  stacks as a context and @maxBound@ as a limit for the call stack size.
--  After the analysis it performs a join over all the results at a label.
mfp :: Algorithm a a
mfp mf l = joinall' (getL mf) (mfp' mf l)

-- |See @mfp@. Takes a value @k@ as the call stack limit.
mfpk :: Int -> Algorithm a a
mfpk k mf l = joinall' (getL mf) (mfpk' k mf l)

-- * Embelished Maximal Fixed-Point (EMFP) Analysis.

-- |See @mfp@. Returns the pointwise analysis instead of joining all results.
mfp' :: Algorithm a (Context CallStack a)
mfp' mf l = mfpk' maxBound mf l

-- |See @mfp@. Takes a value @k@ as the call stack limit, and returns the
--  pointwise analysis instead of joining all results.
mfpk' :: Int -> Algorithm a (Context CallStack a)
mfpk' k mf | isForwards  mf = mfpO mf
           | isBackwards mf = mfpI mf
  where
  mfpI mf l = fixMFP k mf (mkWorkList mf) (mkAnalysis mf) l
  mfpO mf l = let tr = applyT' mf l in tr (mfpI mf l)

-- |A worklist is a list of flows still to examine.
type WorkList = [Flow]

-- |Compute the maximal fixed point for an MF.
fixMFP :: Int -> MF a -> WorkList -> Analysis (Context CallStack a) -> Analysis (Context CallStack a)
fixMFP k mf [    ] mfp = mfp
fixMFP k mf (w:ws) mfp = let

  -- import: refines as (<:), join as (\/) and k-cons as (!:)
  x <: y  = refines (getL' mf) x y
  x \/ y  = join (getL' mf) x y
  x !: xs = kcons k x xs

  in case w of

    -- intraprocedural analysis:
    Intra a b -> let

      -- data: new analysis at l, old analysis at l'
      mfpA  = applyT' mf a (mfp a)
      mfpB  = mfp b

      -- recursive case: compute new worklist and analysis
      mfp' k = if k == b then mfpA \/ mfpB else mfp k
      ws'    = filter (`flowsFrom` b) (mkWorkList mf) ++ ws

      in if mfpA <: mfpB
            then fixMFP k mf ws' mfp'
            else fixMFP k mf ws  mfp

    -- interprocedural analysis:
    Inter a b -> let

      -- data: find the call or retn flow in question
      (isCall,c,r,n,vals) = findCallOrRetn mf a b

      -- data: find the args passed to the procedure call
      args = getArgs mf n vals

      -- recursive case: compute new worklist and analysis
      ws' = filter (`flowsFrom` b) (mkWorkList mf) ++ ws

      in if isCall
          then let -- for call flow, do:

            -- 1. get whatever is the current analysis at the call/entry-site
            mfpC1 = mfp a -- note: a == c
            mfpN  = mfp b -- note: b == n

            -- 2. move all of the call analysis one place deeper into the call stack
            mfpC2 = Context { used = used', at = at' }
              where
              used'      = map (c !:) (used mfpC1)
              at' [    ] = bottom (getL mf)
              at' (x:xs)
                | x /= c = bottom (getL mf)
                | x == c = mfpC1 `at` xs

            -- 3. filter out the arguments/return by assignments
            mfpC3 = procEntry mf args mfpC2

            -- 4. store the join of both analyses as the new analysis at n
            mfp' k = if k == b then mfpC3 \/ mfpN else mfp k

          in fixMFP k mf ws' mfp'
          else let -- for retn flow, do:

            -- 1. get whatever is the current analysis at the call/exit/retn-site
            mfpC  = applyT' mf a (mfp c)
            mfpX1 = mfp a -- note: a == x
            mfpR  = mfp b -- note: b == r

            -- 3. filter out the arguments/return by assignments
            mfpX2 = procExit mf args mfpX1

            -- 2. move all of the exit analysis one place up in the call stack
            mfpX3 = Context { used = used', at = at' }
              where
              used'  = used mfpC `L.union` map tail (used mfpX2)
              at' xs = mfpC `at` xs \/ (mfpX2 `at` (c : xs))

              -- note: import due to usage of regular join above
              x \/ y = join (getL mf) x y

            -- 4. store the join of both analyses as the new analysis at n
            mfp' k = if k == b then mfpX3 \/ mfpR else mfp k

          in fixMFP k mf ws' mfp'

-- |Initial worklist of the mfp algorithm.
mkWorkList :: MF a -> WorkList
mkWorkList mf = S.toList (getF mf)

-- |Initial output of the embelished Analysis algorithm.
mkAnalysis :: MF a -> Analysis (Context CallStack a)
mkAnalysis mf l
  | l `S.member` getE mf = getI' mf
  | otherwise            = bottom (getL' mf)

-- |Interprets a procedure exit (with a list of arguments) as assignments
--  of @null@ to those arguments.
procExit :: MF a -> [(Name,AExpr)] -> Context c a -> Context c a
procExit mf args mfpX = foldr (<$>) mfpX unassign_args
  where
  unassign_args = map (getT mf . flip assign ANull) (map fst args)

-- |Interprets a procedure entry (with a list of arguments) as assignments
--  of the given values to those arguments, and an assignment to @null@ to
--  the special @return@ value.
procEntry :: MF a -> [(Name,AExpr)] -> Context c a -> Context c a
procEntry mf args mfpC = foldr (<$>) mfpC (unassign_return : assign_args)
  where
  assign_args = map (getT mf . uncurry assign) args
  unassign_return = getT mf $ assign "return" ANull

  
-- * Contexts

-- |A Context object contains a number of analyses for different
--  contexts (e.g. call stacks), together with a list of the contexts
--  that were observed during the analysis.
data Context c a = Context { used :: [c] , at :: c -> a }

instance (Show c, Show a) => Show (Context c a) where
  show cxt = printf "{ %s}" show''
    where
    show''  = unlines $ L.intersperse ", " $ map show' $ used cxt
    show' c = printf "%s -> %s" (show c) (show $ cxt `at` c)

instance Functor (Context c) where
  fmap f c = c { at = f . at c }


-- |A call stack is a useful kind of context, which stores the procedure
--  calls in an analysis on a stack (with the top element being the most
--  recent procedure call).
type CallStack = [Label]

-- |A variant of cons `(::)` that returns a list of at most length `k`.
kcons :: Int -> a -> [a] -> [a]
kcons 0 _  _     = [ ]
kcons _ x [ ]    = [x]
kcons k x (y:ys) = x : kcons (k - 1) y ys


-- * Lifted Monotone Framework Operations

-- |Lifts a lattice on a property space to a lattice on a
--  context-sensitive property space.
getL' :: (Eq c) => MF a -> Lattice (Context c a)
getL' mf = Lattice { join = join', refines = refines', bottom  = bottom' }
  where
  -- join: combine all used contexts, join pointwise
  join' c1 c2
    = Context { used = union_used c1 c2 , at   = pointwise join c1 c2 }

  -- refines: check if c1 refines c2 over all used contexts
  refines' c1 c2
    = L.any (pointwise refines c1 c2) (union_used c1 c2)

  -- bottom: without used contexts, always return bottom
  bottom'
    = Context { used = [] , at = const (bottom (getL mf)) }

  -- utils: applies a lattice operation pointwise, unions the used contexts
  pointwise f c1 c2 c = f (getL mf) (c1 `at` c) (c2 `at` c)
  union_used  c1 c2   = used c1 `L.union` used c2

-- |Computes the join over all results in a context, effectively flattening
--  the contexts.
joinall' :: Lattice a -> Context c a -> a
joinall' lat cxt = joinall lat $ map (cxt `at`) (used cxt)

-- |Computes the extremal value of an MF based on the callstack
--  in the context.
getI' :: MF a -> Context CallStack a
getI' mf = Context
  { used = [[]]
  , at = \c -> case c of
      [] -> getI mf
      cs -> bottom (getL mf)
  }

-- |Computes a lifted transfer function that works pointwise.
getT' :: MF a -> Transfer (Context CallStack a)
getT' mf s ca = getT mf s <$> ca

-- |Applies a lifted transfer function that works pointwise.
applyT' :: MF a -> Label -> Context c a -> Context c a
applyT' mf l ca = applyT mf l <$> ca


-- * Utility Functions

-- |Finds either a call or a return statement with a specific label.
findCallOrRetn :: MF a -> Label -> Label -> (Bool,Label,Label,Name,[AExpr])
findCallOrRetn mf a b = case call <|> retn of
  Just (isCall, Call c r n vals) -> (isCall,c,r,n,vals)
  Nothing -> error ("could not find a call statement associated with "++show a++" or "++show b)
  where
  call = (True ,) <$> findCall mf a
  retn = (False,) <$> findRetn mf b

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
