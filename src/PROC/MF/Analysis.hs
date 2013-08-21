{-# LANGUAGE ExistentialQuantification #-}
module PROC.MF.Analysis where

import Prelude hiding (init)

import PROC.Base
import PROC.MF.Flowable

import Data.Monoid ((<>))

import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Map as M

import Text.Printf (printf)

-- |Runs an Analysis analysis on a program and prints the result to stdout.
analyseAndPrint :: (Show b) => Algorithm a b -> (Prog -> MF a) -> Prog -> IO ()
analyseAndPrint = ((putStrLn . ) . ) . analyseAndShow

-- |Runs an Analysis analysis on a program and shows the result.
analyseAndShow :: (Show b) => Algorithm a b -> (Prog -> MF a) -> Prog -> String
analyseAndShow alg mf p = showAnalysis (S.toList $ labels p) (analyse alg mf p)

-- |Show instances of Analysis--which are functions--for a limited
--  number of inputs.
showAnalysis :: (Show a) => [Label] -> Analysis a -> String
showAnalysis ls analysis = unlines $ map (\l -> printf "%d: %s" l (show $ analysis l)) ls

-- |An analysis is a function that contains information for all valid labels.
type Analysis a = Label -> a

-- |A method is an algorithm for obtaining an analysis from an MF and a program.
type Algorithm a b = MF a -> Analysis b

-- |Runs an Analysis analysis on a program at a certain label.
analyse :: Algorithm a b -> (Prog -> MF a) -> Prog -> Analysis b
analyse alg mkMF p@(Prog d s) l
  | S.member l (labels p) = alg (mkMF p) l
  | otherwise             = error ("no statement with label " ++ show l)

-- * Monotone Frameworks

-- |Type for transfer functions in an MF.
type Transfer a = Stmt -> a -> a

data Direction = Forwards | Backwards

isForwards :: MF a -> Bool
isForwards mf = case getDirection mf of Forwards -> True ; _ -> False

isBackwards :: MF a -> Bool
isBackwards mf = case getDirection mf of Backwards -> True ; _ -> False

data MF a = MF
  { getI         :: a                          -- ^ extremal values
  , getE         :: Set Label                  -- ^ extremal labels
  , getF         :: Set Flow                   -- ^ control flow for analysis
  , getIF        :: Set InterFlow              -- ^ inter-procedural flow
  , getL         :: Lattice a                  -- ^ lattice on property space
  , getT         :: Transfer a                 -- ^ transfer function
  , getD         :: FTable                     -- ^ procedure declarations
  , getDirection :: Direction                  -- ^ direction of the analysis
  , getBlocks    :: Set Stmt                   -- ^ blocks in a program
  }

-- * Lattices

data Lattice a = Lattice
  { join    :: a -> a -> a
  , refines :: a -> a -> Bool
  , bottom  :: a
  }

-- |Joins a list of values.
joinall :: Lattice a -> [a] -> a
joinall l = foldr (join l) (bottom l)

-- * MF transformers

-- |Easily make MF's for backwards analyses.
forwards :: Prog -> MF a -> MF a
forwards p@(Prog _ s) mf = mf
  { getE         = S.singleton (init s)
  , getF         = progFlow p
  , getIF        = progInterFlow p
  , getDirection = Forwards
  }

-- |Easily make MF's for forwards analyses.
backwards :: Prog -> MF a -> MF a
backwards p@(Prog _ s) mf = mf
  { getE         = final s
  , getF         = progFlowR p
  , getIF        = progInterFlowR p
  , getDirection = Backwards
  }

-- |Type for @kill@ functions of distributive MF's.
type Kill a = Stmt -> a -> a

-- |Type for @gen@ functions of distributive MF's.
type Gen a = Stmt -> a

-- |Easily make distributive monotone frameworks.
distributive :: (Ord a) => Kill (Set a) -> Gen (Set a) -> MF (Set a) -> MF (Set a)
distributive kill gen mf = mf { getT = transfer }
  where
  transfer s rs = (rs \\ killed) <> genned
    where
    killed = kill s (bottom $ getL mf)
    genned = gen s

embelished :: Prog -> MF a -> MF a
embelished p@(Prog d _) mf = mf
  { getD      = mkFTable d
  , getBlocks = blocks p
  }

-- |Empty monotone framework.
framework :: MF a
framework = MF
  { getI          = error "uninitialized property 'I'"
  , getE          = error "uninitialized property 'E' (apply 'backwards' or 'forwards')"
  , getF          = error "uninitialized property 'F' (apply 'backwards' or 'forwards')"
  , getIF         = error "uninitialized property 'IF' (apply 'backwards' or 'forwards')"
  , getL          = error "uninitialized property 'L'"
  , getT          = error "uninitialized property 'T'"
  , getD          = error "uninitialized property 'D' (apply 'embelished')"
  , getDirection  = error "uninitialized property 'direction'"
  , getBlocks     = error "uninitialized property 'blocks' (apply 'embelished')"
  }

-- |Applies a transfer function for a nested block.
applyT :: MF a -> Label -> a -> a
applyT mf l = getT mf (select l (getBlocks mf))

-- |Computes a list of name/value pairs for a procedure call based upon
--  the call-site information for this call.
getArgs :: MF a -> Name -> [AExpr] -> [(Name,AExpr)]
getArgs mf n vals = case M.lookup n (getD mf) of
  Just (Decl _ names _) -> zip names vals
  Nothing               -> error ("undefined function \"" ++ show n ++ "\"")

