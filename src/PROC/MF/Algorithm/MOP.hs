module PROC.MF.Algorithm.MOP (mop,mopk,vkpaths) where

import PROC.Base
import PROC.MF.Flowable
import PROC.MF.Analysis

import Debug.Trace (traceShow)

import Data.Maybe (mapMaybe)
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.List as L (init)

-- * Meet Over all Paths (MOP) Analysis

-- |Performs a MOP-k analysis with a large value for @k@.
mop :: Algorithm a a
mop = mopk maxBound

-- |Performs a Meet Over all Paths where paths are limited to @k@ repetitions
--  of the same label. MOP-k is therefore "suitable" for the analysis of looping and
--  recursive programs, as these have an infinite number of paths.
-- 
--  Note: MOP-k is not a safe analysis algorithm, as it doesn't necessarily err on the
--  safe side when presented with a looping or recursive program. This is because some
--  critical paths may not be generated with a low enough value for @k@. It's use for
--  the analysis of these programs should therefore be dissuaded.
mopk :: Int -> Algorithm a a
mopk k mf l
  | k < 2     = error "mopk: k should be a large integer >= 3"
  | otherwise = joinall (getL mf) . map ($ getI mf) $ tr
     where tr = map (getTforPath mf) (vkpaths k mf l)
  
-- |Generates all valid paths through a program with up to @k@ reperititons
--  of the same label.
vkpaths :: Int -> MF a -> Label -> [Path]
vkpaths k mf l = concatMap (\i -> vkpaths' k intraflow interflow [l] i l) extremals
  where
  vkpaths'  | isForwards  mf = vkpathsO
            | isBackwards mf = vkpathsI
  intraflow = mapMaybe toIntra . S.toList $ getF mf
  interflow = S.toList $ getIF mf
  extremals = S.toList $ getE  mf

-- |Composes the transfer functions along a path.
getTforPath :: MF a -> Path -> a -> a
getTforPath mf [    ] = id
getTforPath mf (l:ls) = getTforPath mf ls . applyT mf l

-- |A path is an ordered list of visited program labels.
type Path = [Label]

-- |Compute all valid paths upto (not including) @l@.
vkpathsI :: Int -> [IntraFlow] -> [InterFlow] -> Path -> Label -> Label -> [Path]
vkpathsI = (((((map L.init . ) . ) . ) . ) . ) . vkpathsO

-- |Compute all valid paths upto (and including) @l@.
vkpathsO :: Int -> [IntraFlow] -> [InterFlow] -> Path -> Label -> Label -> [Path]
vkpathsO k nfs ifs acc a b
  | a == b    = [acc]
  | otherwise = vintra a b ++ vinter a b
  where
  
  vintra :: Label -> Label -> [Path]
  vintra a b = do
    (c,r) <- filter (\(c,r) -> b == r && nokloop k r acc) nfs
    a2b <- vkpathsO k nfs ifs (c : acc) a c
    return a2b
    
  vinter :: Label -> Label -> [Path]
  vinter a b = do
    (c,n,x,r) <- filter (\(c,_,_,r) -> b == r && nokloop k r acc) ifs
    n2x <- vkpathsO k nfs ifs (x : acc) n x
    a2b <- vkpathsO k nfs ifs (c : n2x) a c
    return a2b

-- |Checks if a path is still allowed to visit a certain label.
nokloop :: Int -> Label -> Path -> Bool
nokloop k l p = count (== l) p <= k
  where
  count :: (a -> Bool) -> [a] -> Int
  count = (length . ) . filter
