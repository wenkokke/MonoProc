module PROC.MF.Analysis.CP where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable

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
refinesCP  Nothing   _        = True
refinesCP  _         Nothing  = False
refinesCP (Just s1) (Just s2) = all (\z -> M.lookup z s1 `refinesZT` M.lookup z s2) keys
  where
  keys = S.toList $ S.union (M.keysSet s1) (M.keysSet s2)

-- |Refining predicate of the Z+T type.
refinesZT :: ZT -> ZT -> Bool
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
  $ embelished (toEnv d)
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
transferCP (Assign _ x a) _ m = do m <- m; i <- evalAE m a; return $ M.insert x i m
transferCP (Skip _)       _ m = m
transferCP (BExpr _ _)    _ m = m
transferCP  other         _ m = error (show other)

evalAE :: Map Name Integer -> AExpr -> ZT
evalAE env a = case a of
  AName n   -> M.lookup n env
  AConst i  -> pure i
  Add e1 e2 -> (+) <$> evalAE env e1 <*> evalAE env e2
  Sub e1 e2 -> (-) <$> evalAE env e1 <*> evalAE env e2
  Mul e1 e2 -> (*) <$> evalAE env e1 <*> evalAE env e2
  Div e1 e2 -> div <$> evalAE env e1 <*> evalAE env e2
  Neg e1    -> negate <$> evalAE env e1
