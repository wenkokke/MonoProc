module PROC.MF.Analysis.CP (mfCP,CP (..),ZT (..)) where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.FreeNames
import qualified PROC.Evaluating as E (evalAE)

import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative ((<$>),liftA2)

import Debug.Trace (trace)
import Text.Printf (printf)

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
data CP = CP (Map Name ZT)
        | Bottom
        deriving (Eq)

instance Show CP where
  show Bottom = "Bottom"
  show (CP m) = show m

-- |Representation of Z+T.
data ZT = Z Integer
        | Top
        deriving (Eq)

instance Show ZT where
  show (Z i) = show i
  show  Top  = "T"

(!) :: Map Name ZT -> Name -> ZT
(!) m n = case M.lookup n m of
  Just zt -> zt
  Nothing -> error ("eval: unbound variable '"++n++"'")

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
refinesCP  _       Bottom = True
refinesCP  Bottom  _      = False
refinesCP (CP s1) (CP s2) = all (\k -> (s1 ! k) `refinesZT` (s2 ! k)) keys
  where
  keys = S.toList $ S.union (M.keysSet s1) (M.keysSet s2)

-- |Refining predicate of the Z+T type.
refinesZT :: ZT -> ZT -> Bool
refinesZT  Top    _     = True
refinesZT  _      Top   = False
refinesZT (Z z1) (Z z2) = True -- z1 == z2

-- |Least upper bound of the CP type.
joinCP :: CP -> CP -> CP
joinCP  s1      Bottom = s1
joinCP  Bottom  s2     = s2
joinCP (CP s1) (CP s2) = CP (M.unionWith joinZT s1 s2)

-- |Least upper bound of Z+T.
joinZT :: ZT -> ZT -> ZT
joinZT  _      Top   = Top
joinZT  Top    _     = Top
joinZT (Z z1) (Z z2) | z1 == z2 = Z z1
                     | otherwise = Top

-- |Transfer function for CP
transferCP :: Transfer CP
transferCP s m = case s of
  (Assign _ x a) -> case m of
                      Bottom -> Bottom
                      CP m   -> CP (M.insert x (evalAE m a) m)
  (Skip _)       -> m
  (BExpr _ _)    -> m
  (Call _ _ _ _) -> m
  (other)        -> error (show other)

evalAE :: Map Name ZT -> AExpr -> ZT
evalAE m a
  | anyTop    = Top
  | otherwise = case E.evalAE (fromZ <$> m) a of
    Left  msg -> error msg
    Right zt  -> Z zt
  where
    anyTop = any isTop (map (m !) fv)
    fv     = S.toList (freeNames a)
