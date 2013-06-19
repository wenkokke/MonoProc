module PROC.Flow where

import PROC.Base
import Text.Printf (printf)
import Control.Monad.Supply

-- * Statement labelling
 
class Labelable a where
  label :: a -> Supply Label a
  runLabel :: a -> a
  runLabel a = evalSupply (label a) [1..]
  
instance (Labelable a) => Labelable [a] where
  label = mapM label
  
instance Labelable Prog where
  label (Prog decls sts) = do decls <- label decls; sts <- label sts; return (Prog decls sts)
  
instance Labelable Decl where
  label (Decl n xs sts)  = do sts <- label sts; return (Decl n xs sts)
  
instance Labelable Stmt where
  label (Skip _)         = do l <- supply; return (Skip l)
  label (Assign _ n ae)  = do l <- supply; return (Assign l n ae)
  label (IfThen _ b t f) = do l <- supply; t <- label t; f <- label f; return (IfThen l b t f)
  label (While _ b w)    = do l <- supply; w <- label w; return (While l b w)
  label (Call _ n ae)    = do l <- supply; return (Call l n ae)
  label (Seq x xs)       = do xs <- label xs; return (Seq x xs)
  
-- * Control-flow analysis

data Flow
  = Intra Label Label
  | Inter Label Label
  deriving (Eq)
  
instance Show Flow where
  show (Intra x y) = printf "(%s,%s)" (show x) (show y)
  show (Inter x y) = printf "(%s;%s)" (show x) (show y)