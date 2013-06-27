module PROC.MF.Flowable where

import Prelude hiding (init)
import PROC.Base
import Text.Printf (printf)
import Data.Set (Set,(\\))
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)
import Data.Map (Map)
import qualified Data.Map as M

import Data.Monoid

data Flow
  = Intra Label Label
  | Inter Label Label
  deriving (Eq,Ord)
  
instance Show Flow where
  show (Intra x y) = printf "(%s,%s)" (show x) (show y)
  show (Inter x y) = printf "(%s;%s)" (show x) (show y)
  
-- |Reverses a @Flow@ tuple.
swap :: Flow -> Flow
swap (Intra a b) = Intra b a
swap (Inter a b) = Inter b a

-- |Uses the declarations in a @Prog@ constructor to create
--  en environment, and passes that to @flow@.
flow' :: Prog -> Set Flow
flow' p@(Prog d _) = flow (toEnv d) p

-- |Uses the declarations in a @Prog@ constructor to create
--  en environment, and passes that to @flowR@.
flowR' :: Prog -> Set Flow
flowR' p@(Prog d _) = flowR (toEnv d) p

-- |Act as @fst@ and @snd@ on @Flow@ tuples, respectively.
from,to :: Flow -> Label
from (Intra l _) = l
from (Inter c _) = c
to   (Intra _ l) = l
to   (Inter _ r) = r

-- |Selects a block with a certain label from a set, and throws
--  and error when either /no block/ has the label, or /multiple blocks/ do.
select :: Label -> Set Stmt -> Stmt
select l = isolated . S.elems . S.filter hasLabel 
  where
  isolated [ ] = error ("no statement with label " ++ show l)
  isolated [x] = x
  isolated  _  = error ("multiple statements with label " ++ show l)
  hasLabel :: Stmt -> Bool
  hasLabel (Assign l' _ _) = l == l'
  hasLabel (BExpr l' _)    = l == l'
  hasLabel (Skip l')       = l == l'
  hasLabel (While b _)     = hasLabel b
  
class Flowable a where
  init   :: a -> Label
  final  :: a -> Set Label
  blocks :: a -> Set Stmt
  labels :: a -> Set Label
  labels  = S.map init . blocks
  flow   :: Env -> a -> Set Flow
  flowR  :: Env -> a -> Set Flow
  flowR e = S.map swap . flow e
  entry  :: Env -> a -> Label
  entry e = isolated . S.elems . S.map to . entry'
    where
    entry'    a  = (S.map (\l -> Intra l (init a)) (labels a)) \\ (flow e a)
    isolated [ ] = error "no isolated entries exist"
    isolated [x] = x
    isolated  _  = error "multiple isolated entries exist"
  exits  :: Env -> a -> Set Label
  exits e = S.map from . exits'
    where
    exits' a = (S.foldMap (\l1 -> S.map (\l2 -> Intra l1 l2) (labels a)) (final a)) \\ (flow e a)

instance Flowable Prog where
  init   (Prog d s) = init s
  final  (Prog d s) = final s
  blocks (Prog d s) = blocks s <> (S.foldMap blocks d)
  labels (Prog d s) = labels s <> (S.foldMap labels d)
  flow e (Prog d s) = flow e s <> (S.foldMap (flow e) d)

instance Flowable Decl where
  init   (Decl _ _ s) = init s
  final  (Decl _ _ s) = final s
  blocks (Decl _ _ s) = blocks s
  labels (Decl _ _ s) = labels s
  flow e (Decl _ _ s) = flow e s
  
instance Flowable Stmt where
  init (Skip l)       = l
  init (Assign l _ _) = l
  init (Seq s1 s2)    = init s1
  init (IfThen b _ _) = init b
  init (While b _)    = init b
  init (BExpr l _)    = l
  init (Call c _ _ _) = c
  
  final (Skip l)          = S.singleton l
  final (Assign l _ _)    = S.singleton l
  final (Seq s1 s2)       = final s2
  final (IfThen _ s1 s2)  = final s1 <> final s2
  final (While b _)       = S.singleton (init b)
  final (Call _ r _ _)    = S.singleton r
  
  blocks s@(Skip _)         = S.singleton s
  blocks s@(Assign _ _ _)   = S.singleton s
  blocks s@(BExpr _ _)      = S.singleton s
  blocks s@(Seq s1 s2)      = blocks s1 <> blocks s2
  blocks s@(IfThen b s1 s2) = blocks b <> blocks s1 <> blocks s2
  blocks s@(While b s1)     = blocks b <> S.singleton s <> blocks s1
  blocks s@(Call _ _ _ _)   = S.singleton s
  
  flow e (Skip _)         = S.empty
  flow e (Assign _ _ _)   = S.empty
  flow e (Seq s1 s2)      = flow e s1 <> flow e s2 <> (S.map (\l -> Intra l (init s2)) (final s1))
  flow e (IfThen b s1 s2) = flow e s1 <> flow e s2 <> (S.map (\s -> Intra (init b) (init s)) (S.fromList [s1,s2]))
  flow e (While b s1)     = flow e s1 <> (S.map (\l -> Intra l (init b)) (S.insert (init s1) (final s1)))
  flow e (Call c r n _)   = case M.lookup n e of
                              Just d  -> S.insert (Inter c (init d)) (S.map (\l -> Inter l r) (final d))
                              Nothing -> error ("no function called " ++ show n)
