module PROC.MF.Analysis.CP (mfCP) where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable
import PROC.Evaluating (VTable)
import qualified PROC.Evaluating as E (VTable,evalAE)

import Text.Printf (printf)
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Control.Applicative (pure,(<$>),(<*>))

-- |Representation of (Var* -> (Z+T))+F
type CP = Maybe (Map Name Integer)

-- |Representation of Z+T.
type ZT = Maybe Integer

-- |The top and bottom elements.
topCP, botCP :: CP
topCP = Just M.empty
botCP = Nothing

-- |Refining predicate of the CP type.
refinesCP :: CP -> CP -> Bool
refinesCP  Nothing   Nothing  = False
refinesCP  Nothing   _        = True
refinesCP  _         Nothing  = False
refinesCP (Just s1) (Just s2) = all (\z -> M.lookup z s1 `refinesZT` M.lookup z s2) keys
  where
  keys = S.toList $ S.union (M.keysSet s1) (M.keysSet s2)

-- |Refining predicate of the Z+T type.
refinesZT :: ZT -> ZT -> Bool
refinesZT  Nothing   Nothing  = False
refinesZT  _         Nothing  = True
refinesZT  Nothing   _        = False
refinesZT (Just z1) (Just z2) = z1 == z2

-- |Least upper bound of the CP type.
joinCP :: CP -> CP -> CP
joinCP  s1        Nothing  = s1
joinCP  Nothing   s2       = s2
joinCP (Just s1) (Just s2) = Just $ S.fold (\x -> insert x $ f1 x `joinZT` f2 x) M.empty keys
  where
  insert x zt s = maybe s (\z -> M.insert x z s) zt
  f1 x = M.lookup x s1
  f2 x = M.lookup x s2
  keys = S.union (M.keysSet s1) (M.keysSet s2)
  
-- |Least upper bound of Z plus TOP.
joinZT :: ZT -> ZT -> ZT
joinZT  z1        Nothing  = z1
joinZT  Nothing   z2       = z2
joinZT (Just z1) (Just z2) | z1 == z2  = Just z1 
                           | otherwise = Nothing
mfCP :: Prog -> MF CP
mfCP (Prog d s)
  = forwards s
  $ embelished (mkFTable d)
  $ framework
  { getI = topCP
  , getL = Lattice
    { join    = joinCP
    , refines = refinesCP
    , bottom  = botCP
    }
  , getT = transferCP
  }

transferCP :: Transfer CP
transferCP s l m = case select l (blocks s) of
  (Assign _ x a) -> do m <- m; i <- evalAE m a; return $ M.insert x i m
  (Skip _)       -> m
  (BExpr _ _)    -> m
  (other)        -> error (show other)

evalAE :: VTable -> AExpr -> ZT
evalAE env a = case E.evalAE env a of
  Left  _ -> Nothing
  Right i -> Just i
  