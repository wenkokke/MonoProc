module PROC.Evaluating where

import PROC.Base
import PROC.MF.Analysis
import PROC.MF.UsedNames
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Control.Applicative (pure,(<$>),(<*>))
import Control.Monad.Error (throwError)

type VTable = Map Name Integer
type PROC a = Either String a

class (UsedNames a) => Eval a where

evalProg :: Prog -> PROC VTable
evalProg (Prog d s) = evalStmt fs s vs
  where
  fs = mkFTable d
  vs = M.empty

evalStmt :: FTable -> Stmt -> VTable -> PROC VTable
evalStmt fs s vs = case s of
  Skip _                    -> return vs
  Assign _ x ae             -> do i <- evalAE vs ae; return $ M.insert x i vs
  IfThen (BExpr _ be) s1 s2 -> do b <- evalBE vs be; if b then evalStmt fs s1 vs else evalStmt fs s2 vs
  While (BExpr _ be) s1     -> do b <- evalBE vs be; if b then evalStmt fs s1 vs >>= evalStmt fs s else return vs
  Seq s1 s2                 -> evalStmt fs s1 vs >>= evalStmt fs s2
  Call _ _ n aes            -> case M.lookup n fs of
    Just (Decl _ xs s)      ->

      do evaled      <- mapM (evalAE vs) aes;
         let args     = M.fromList (zip xs evaled)
         let shadowed = foldr (\x r -> maybe r (\i -> M.insert x i r) (M.lookup x vs)) M.empty xs
         let callenv  = args <> vs
         retenv      <- evalStmt fs s callenv
         return $ shadowed <> (foldr M.delete retenv xs)

    Nothing                 -> throwError ("eval: unknown function '"++n++"'")

evalBE :: VTable -> BExpr -> PROC Bool
evalBE vs b = case b of
  BConst e1 -> pure e1
  Lt  e1 e2 -> (<) <$> evalAE vs e1 <*> evalAE vs e2
  Lte e1 e2 -> (<=) <$> evalAE vs e1 <*> evalAE vs e2
  Gt  e1 e2 -> (>) <$> evalAE vs e1 <*> evalAE vs e2
  Gte e1 e2 -> (>=) <$> evalAE vs e1 <*> evalAE vs e2
  Eq  e1 e2 -> (==) <$> evalAE vs e1 <*> evalAE vs e2
  Neq e1 e2 -> (/=) <$> evalAE vs e1 <*> evalAE vs e2
  Not e1    ->  not  <$> evalBE vs e1

evalAE :: VTable -> AExpr -> PROC Integer
evalAE vs a = case a of
  ANull     -> throwError ("eval: encountered null")
  AName n   -> case M.lookup n vs of
                  Just i  -> pure i
                  Nothing -> throwError ("eval: unbound variable '"++n++"'")
  AConst i  -> pure i
  Add e1 e2 -> (+) <$> evalAE vs e1 <*> evalAE vs e2
  Sub e1 e2 -> (-) <$> evalAE vs e1 <*> evalAE vs e2
  Mul e1 e2 -> (*) <$> evalAE vs e1 <*> evalAE vs e2
  Div e1 e2 -> div <$> evalAE vs e1 <*> evalAE vs e2
  Neg e1    -> negate <$> evalAE vs e1
