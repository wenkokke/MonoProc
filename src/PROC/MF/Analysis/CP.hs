module PROC.MF.Analysis.CP (mfCP,CP (..),ZT (..)) where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable
import PROC.MF.FreeNames
import PROC.Evaluating (VTable)
import qualified PROC.Evaluating as E (VTable,evalAE)

import Text.Printf (printf)
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Control.Applicative (pure,(<$>),(<*>))

mfCP :: Prog -> MF CP
mfCP p
  = forwards p
  $ embelished p
  $ framework
  { getI = topCP
  , getL = Lattice
    { join    = joinCP
    , refines = refinesCP
    , bottom  = botCP
    }
  , getT = transferCP
  }

-- |Representation of (Var* -> (Z+T))+F
data CP = CP (Map Name ZT) | Bottom
  deriving (Eq)
  
instance Show CP where
  show Bottom = "Bottom"
  show (CP m) = show m

-- |Representation of Z+T.
data ZT = Z Integer | Top
  deriving (Eq)
  
instance Show ZT where
  show (Z i) = show i
  show  Top  = "T"
  
fromZ :: ZT -> Integer
fromZ (Z i) = i

isTop :: ZT -> Bool
isTop Top = True
isTop _   = False

-- |The top and bottom elements.
topCP, botCP :: CP
topCP = CP M.empty
botCP = Bottom

-- |Refining predicate of the CP type.
refinesCP :: CP -> CP -> Bool
refinesCP  _       Bottom = False
refinesCP  Bottom  _      = True
refinesCP (CP s1) (CP s2) = all (\z -> M.lookup z s1 `refinesMZT` M.lookup z s2) keys
  where
  keys = S.toList $ S.union (M.keysSet s1) (M.keysSet s2)
  
-- |Refining predicate of maybe Z+T.
refinesMZT :: Maybe ZT -> Maybe ZT -> Bool
refinesMZT  Nothing   _        = False
refinesMZT  _         Nothing  = True
refinesMZT (Just z1) (Just z2) = z1 `refinesZT` z2

-- |Refining predicate of the Z+T type.
refinesZT :: ZT -> ZT -> Bool
refinesZT  Top    _     = False
refinesZT  _      Top   = True
refinesZT (Z z1) (Z z2) = z1 == z2

-- |Least upper bound of the CP type.
joinCP :: CP -> CP -> CP
joinCP  s1      Bottom = s1
joinCP  Bottom  s2     = s2
joinCP (CP s1) (CP s2) = CP $ S.fold (\x -> insert x $ f1 x `joinMZT` f2 x) M.empty keys
  where
  insert x zt s = maybe s (\z -> M.insert x z s) zt
  f1 x = M.lookup x s1
  f2 x = M.lookup x s2
  keys = S.union (M.keysSet s1) (M.keysSet s2)
  
-- |Least upper bound of maybe Z+T.
joinMZT :: Maybe ZT -> Maybe ZT -> Maybe ZT
joinMZT  Nothing   z2       = z2
joinMZT  z1        Nothing  = z1
joinMZT (Just z1) (Just z2) = Just (z1 `joinZT` z2)
  
-- |Least upper bound of Z+T.
joinZT :: ZT -> ZT -> ZT
joinZT  z1     Top   = z1
joinZT  Top    z2    = z2
joinZT (Z z1) (Z z2) | z1 == z2  = Z z1 
                     | otherwise = Top

transferCP :: Transfer CP
transferCP s l m = case select l (blocks s) of
  (Assign _ x a) -> case m of
                      Bottom -> Bottom
                      CP m   -> CP $ case (M.lookup x m, evalAE m a) of
                                  (Nothing, Nothing) -> m
                                  (Nothing, Just zt) -> M.insert x zt m
                                  (Just zt, Nothing) -> M.insert x Top m
                                  (Just z1, Just z2) -> M.insert x (z1 `joinZT` z2) m
  (Skip _)       -> m
  (BExpr _ _)    -> m
  (other)        -> error (show other)

evalAE :: Map Name ZT -> AExpr -> Maybe ZT
evalAE m a | anyTop    = Just Top
           | otherwise = Z <$> toMaybe (E.evalAE safeCP a)
  where
  toMaybe (Left  _) = Nothing
  toMaybe (Right x) = Just x
  safeCP  = fromZ <$> m
  anyTop  = any isTop allCPs
  allCPs  = mapMaybe (\x -> M.lookup x m) (S.toList $ freeNames a)
  