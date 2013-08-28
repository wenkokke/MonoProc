module PROC.MF.Algorithm.MOP (mop,mopk) where

import PROC.Base
import PROC.MF.Flowable
import PROC.MF.Analysis
import PROC.MF.Algorithm

import Data.Maybe (mapMaybe)
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
--
--  TODO: MOP does not play entirely nice with procedure calls. It does not interpret
--  procedure entries as assignments to parameters, and does not interpret procedure
--  exits as null assignments to these parameters (nor does it restore from the call-site).
--  The reason I have not yet implemented this is because this will require some serious
--  modification to the valid k-paths function @vkpaths@, in order to mark procedure entries
--  and exits. I guess this is where deciding not to give them labels comes back to bite me.
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
  interflow = S.toList $ getIF mf
  intraflow = intras ++ entries
    where
    intras  = mapMaybe toIntra . S.toList $ getF mf
    entries = map (\(c,n,_,_) -> (c,n)) interflow   -- nb: we add entries to be able to
                                                    --     paths into procedure bodies.
  extremals = S.toList $ getE  mf


-- |Composes the transfer functions along a path.
getTforPath :: MF a -> Path -> a -> a
getTforPath mf [    ] = id
getTforPath mf (l:ls) = getTforPath mf ls . applyT mf l

-- |Applies a transfer function for a nested block, and includes tranfer
--  into and out of procedure calls.
--  Note: this function is different from the @applyT@ defined in the MFP module.
applyT :: MF a -> Label -> a -> a
applyT mf l = getT mf stmt . proc stmt
  where
  stmt = select l (getBlocks mf)
  proc (Call c r n vals)
       | l == c = procEntry mf args
       | l == r = procExit mf args
       where
       args = getArgs mf n vals
  proc _ = id

-- |Interprets a procedure entry (with a list of arguments) as assignments
--  of the given values to those arguments, and an assignment to @null@ to
--  the special @return@ value.
--  Note: this function is different from the @procEntry@ defined in the MFP module.
procEntry :: MF a -> [(Name,AExpr)] -> a -> a
procEntry mf args mfpC = foldr ($) mfpC (unassign_return : assign_args)
  where
  assign_args = map (getT mf . uncurry assign) args
  unassign_return = getT mf $ assign "return" ANull

-- |Interprets a procedure exit (with a list of arguments) as assignments
--  of @null@ to those arguments.
--  Note: this function is different from the @procExit@ defined in the MFP module.
procExit :: MF a -> [(Name,AExpr)] -> a -> a
procExit mf args mfpX = foldr ($) mfpX unassign_args
  where
  unassign_args = map (getT mf . flip assign ANull) (map fst args)

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
