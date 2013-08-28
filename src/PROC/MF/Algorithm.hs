-- |Contains utility functions used in all algorithm implementations.
module PROC.MF.Algorithm where

import PROC.Base (Name,AExpr,Decl (..))
import PROC.MF.Analysis (MF, getD)

import qualified Data.Map as M

-- |Computes a list of name/value pairs for a procedure call based upon
--  the call-site information for this call.
getArgs :: MF a -> Name -> [AExpr] -> [(Name,AExpr)]
getArgs mf n vals = case M.lookup n (getD mf) of
  Just (Decl _ names _) -> zip names vals
  Nothing               -> error ("undefined function \"" ++ show n ++ "\"")
