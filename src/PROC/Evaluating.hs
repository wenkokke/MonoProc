module PROC.Evaluating where

import PROC.Base
import PROC.MF.Analysis
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative (pure,(<$>),(<*>))
import Control.Monad.Error

type VTable = Map Name Integer
type PROC a = Either String a

class (UsedNames a) => Eval a where
  eval :: a -> VTable -> PROC VTable
  runEval :: a -> PROC VTable
  runEval x = eval x (M.fromList $ zip (S.toList $ usedNames x) (repeat 0))
  
instance Eval Prog where
  eval p@(Prog d s) env = eval s env

instance Eval Stmt where
  eval s env = case s of
    Skip _                    -> return env
    Assign _ x ae             -> do i <- evalAE env ae; return $ M.insert x i env
    IfThen (BExpr _ be) s1 s2 -> do b <- evalBE env be; if b then eval s1 env else eval s2 env
    While (BExpr _ be) s1     -> do b <- evalBE env be; if b then eval s1 env >>= eval s else return env
    Seq s1 s2                 -> eval s1 env >>= eval s2
    
evalBE :: VTable -> BExpr -> PROC Bool
evalBE env b = case b of
  BConst e1 -> pure e1
  Lt  e1 e2 -> (<)  <$> evalAE env e1 <*> evalAE env e2
  Lte e1 e2 -> (<=) <$> evalAE env e1 <*> evalAE env e2
  Gt  e1 e2 -> (>)  <$> evalAE env e1 <*> evalAE env e2
  Gte e1 e2 -> (>=) <$> evalAE env e1 <*> evalAE env e2
  Eq  e1 e2 -> (==) <$> evalAE env e1 <*> evalAE env e2
  Neq e1 e2 -> (/=) <$> evalAE env e1 <*> evalAE env e2
  Not e1    -> not  <$> evalBE env e1

evalAE :: VTable -> AExpr -> PROC Integer
evalAE env a = case a of
  AName n   -> case M.lookup n env of
                  Just i  -> pure i
                  Nothing -> throwError ("unbound variable '"++n++"'")
  AConst i  -> pure i
  Add e1 e2 -> (+) <$> evalAE env e1 <*> evalAE env e2
  Sub e1 e2 -> (-) <$> evalAE env e1 <*> evalAE env e2
  Mul e1 e2 -> (*) <$> evalAE env e1 <*> evalAE env e2
  Div e1 e2 -> div <$> evalAE env e1 <*> evalAE env e2
  Neg e1    -> negate <$> evalAE env e1
