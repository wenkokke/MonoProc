module PROC.MF.Flowable where

import Prelude hiding (init)
import PROC.Base
import Text.Printf (printf)
import Data.Set (Set)
import qualified Data.Set as S

import Data.Monoid

data Flow
  = Intra Label Label
  | Inter Label Label
  deriving (Eq,Ord)
  
instance Show Flow where
  show (Intra x y) = printf "(%s,%s)" (show x) (show y)
  show (Inter x y) = printf "(%s;%s)" (show x) (show y)
  
swap :: Flow -> Flow
swap (Intra a b) = Intra b a
  
class Flowable a where
  init   :: a -> Label
  final  :: a -> Set Label
  blocks :: a -> Set Stmt
  labels :: a -> Set Label
  labels = S.map init . blocks
  flow   :: a -> Set Flow
  flowR  :: a -> Set Flow
  flowR  = S.map swap . flow
  
instance Flowable Prog where
  init   (Prog decls stmt) = init stmt
  final  (Prog decls stmt) = final stmt
  blocks (Prog decls stmt) = blocks stmt
  labels (Prog decls stmt) = labels stmt
  flow   (Prog decls stmt) = flow stmt
  
instance Flowable Stmt where
  init (Skip l)       = l
  init (Assign l _ _) = l
  init (Seq s1 s2)    = init s1
  init (IfThen b _ _) = init b
  init (While b _)    = init b
  init (BExpr l _)    = l        
  
  final (Skip l)          = S.singleton l
  final (Assign l _ _)    = S.singleton l
  final (Seq s1 s2)       = final s2
  final (IfThen _ s1 s2)  = final s1 <> final s2
  final (While b _)       = S.singleton (init b)
  
  blocks s@(Skip _)         = S.singleton s
  blocks s@(Assign _ _ _)   = S.singleton s
  blocks s@(Seq s1 s2)      = blocks s1 <> blocks s2
  blocks s@(IfThen _ s1 s2) = blocks s1 <> blocks s2
  blocks s@(While _ s1)     = S.singleton s <> blocks s1
  
  flow (Skip _)         = S.empty
  flow (Assign _ _ _)   = S.empty
  flow (Seq s1 s2)      = flow s1 <> flow s2 <> (S.map (\l -> Intra l (init s2)) (final s1))
  flow (IfThen b s1 s2) = flow s1 <> flow s2 <> (S.map (\s -> Intra (init b) (init s)) (S.fromList [s1,s2]))
  flow (While b s1)     = flow s1 <> (S.map (\l -> Intra l (init b)) (S.insert (init s1) (final s1)))
                            
  