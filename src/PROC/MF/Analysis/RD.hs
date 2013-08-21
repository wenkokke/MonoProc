{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module PROC.MF.Analysis.RD (mfRD,RD (..)) where

import Prelude hiding (init)
import PROC.Base
import PROC.MF.Analysis
import PROC.MF.Flowable
import PROC.MF.FreeNames

import Text.Printf (printf)
import Data.Monoid ((<>))
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

import Text.ParserCombinators.UU ((<$),(<$>),(<|>),pListSep)
import Text.ParserCombinators.UU.BasicInstances (Parser,pSym)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.Utils (runParser,pComma,pNatural,pBraces)

-- * Monotone Framework Instance

-- |Monotone Framework for Reached-Definition Analysis.
--  The intuition of this analysis is to find out which assignments
--  have an effect on the program, i.e. which assignments are "reached".
--  For instance, when a variable "x" is assigned to, and then immediately
--  reassigned, the initial value is never used.
mfRD :: Prog -> MF (Set RD)
mfRD p
  = forwards p
  $ distributive killRD genRD
  $ embelished p
  $ framework
  { getI = S.map (\x -> RD x Nothing) (freeNames p)
  , getL = Lattice
    { join    = S.union
    , refines = flip S.isProperSubsetOf
    , bottom  = S.empty
    }
  }

killRD :: Stmt -> Set RD -> Set RD
killRD (Assign _ x _) bot = S.insert (RD x Nothing) (S.filter (\(RD x' _) -> x == x') bot)
killRD (Skip _)        _  = S.empty
killRD (BExpr _ _)     _  = S.empty
killRD (Call _ _ _ _)  _  = S.empty

genRD :: Stmt -> Set RD
genRD (Assign l x _) = S.singleton (RD x (Just l))
genRD (Skip _)       = S.empty
genRD (BExpr _ _)    = S.empty
genRD (Call _ _ _ _) = S.empty

-- * Reached-Definitions Type

-- |Represents the information returned by Reached-Definition Analysis.
--  It stores whether or not a variable has been assigned to, and if,
--  in what label. For instance, @{x,?}@ means that the variable @x@ has
--  not yet been assigned to, whereas @{y,5}@ has (at this point in the
--  program) been assigned a value in label @5@.
data RD = RD Name (Maybe Label)

instance Show RD where
  show (RD x Nothing ) = printf "{%s,?}" x
  show (RD x (Just l)) = printf "{%s,%d}" x l
  
instance Eq RD where
  (RD x _) == (RD y _) = x == y

instance Ord RD where
  compare (RD x _) (RD y _) = compare x y

