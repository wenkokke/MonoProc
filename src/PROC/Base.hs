module PROC.Base where

import Text.Printf (printf)
import UU.Pretty

data Prog
  = Prog [Decl] [Stmt]
  deriving (Eq)
  
data Decl
  = Decl Name [Name] [Stmt]
  deriving (Eq)
  
data Stmt
  = Skip
  | Assign Name AExpr
  | IfThen BExpr [Stmt] [Stmt]
  | While BExpr [Stmt]
  | Call Name [AExpr]
  deriving (Eq)
  
data AExpr
  = AName Name
  | AConst Integer
  | Add AExpr AExpr
  | Sub AExpr AExpr
  | Mul AExpr AExpr
  | Div AExpr AExpr
  | Neg AExpr
  deriving (Eq)

data BExpr
  = BConst Bool
  | Lt  AExpr AExpr
  | Lte AExpr AExpr
  | Gt  AExpr AExpr
  | Gte AExpr AExpr
  | Eq  AExpr AExpr
  | Neq AExpr AExpr
  | Not BExpr
  deriving (Eq)
  
type Name
  = String

-- * Printing programs

instance Show Prog where
  show = show . pp
  
instance PP Prog where
  pp (Prog decls stmts) =
    vlist (map pp stmts)
      >-< vlist (map pp decls)
  
-- * Printing declarations

instance Show Decl where
  show = show . pp 
  
instance PP Decl where
  pp (Decl n xs body) =
    text n >|< (pp_parens_list 80 $ map pp xs) >#<
      pp_block "{\n" "\n}" "" (map (indent 2) body)
  
-- * Printing statements
  
instance Show Stmt where
  show = show . pp
  
instance PP Stmt where
  pp (Skip)               = text "skip" >|< text ";"
  pp (Assign "return" a)  = text "return" >#< pp a >|< text ";"
  pp (Assign n a)         = text n >#< text "=" >#< pp a >|< text ";"
  pp (IfThen b t [Skip])  = text "if" >#< pp b >#<
                              pp_block "{\n" "\n}" "" (map (indent 2) t)
  pp (IfThen b t f)       = text "if" >#< pp b >#<
                              pp_block "{\n" "\n}" "" (map (indent 2) t)
                              >#< "else" >#<
                              pp_block "{\n" "\n}" "" (map (indent 2) f)
  pp (While b l)          = text "while" >#< pp b >#<
                              pp_block "{\n" "\n}" "" (map (indent 2) l)
  pp (Call n xs)          = text n >|< (pp_parens_list 80 $ map pp xs) >|< text ";"
  
-- * Printing arithmetic expressions
  
instance Show AExpr where
  show = show . pp
  
instance PP AExpr where
  pp (AName n)  = text n
  pp (AConst i) = text (show i)
  pp (Neg x)    = text "-" >#< wrap x
  pp (Add x y)  = wrap x >#< text "+" >#< wrap y
  pp (Sub x y)  = wrap x >#< text "-" >#< wrap y
  pp (Mul x y)  = wrap x >#< text "*" >#< wrap y
  pp (Div x y)  = wrap x >#< text "/" >#< wrap y
  
instance Wrap AExpr where
  wrap a@(AName _)  = pp a
  wrap a@(AConst _) = pp a
  wrap a            = pp_parens (pp a)
  
-- * Printing boolean expressions
  
instance Show BExpr where
  show = show . pp

instance PP BExpr where
  pp (BConst True)  = text "true"
  pp (BConst False) = text "false"
  pp (Lt  x y)      = pp x >#< text "<"  >#< pp y
  pp (Lte x y)      = pp x >#< text "<=" >#< pp y
  pp (Gt  x y)      = pp x >#< text ">"  >#< pp y
  pp (Gte x y)      = pp x >#< text ">=" >#< pp y
  pp (Eq  x y)      = pp x >#< text "==" >#< pp y
  pp (Neq x y)      = pp x >#< text "!=" >#< pp y
  pp (Not b)        = text "~" >#< wrap b

instance Wrap BExpr where
  wrap = pp_parens . pp
  
class Wrap a where
  wrap :: a -> PP_Doc
