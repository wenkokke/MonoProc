module PROC.Base where

import UU.Pretty
import Text.Printf (printf)

import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Foldable as S (foldMap)

data Prog
  = Prog [Decl] Stmt
  deriving (Eq,Ord)
  
data Decl
  = Decl Name [Name] Stmt
  deriving (Eq,Ord)
  
data Stmt
  = Skip    Label
  | BExpr   Label BExpr
  | Assign  Label Name AExpr
  | IfThen  Stmt Stmt Stmt
  | While   Stmt Stmt
  | Call    Label Name [AExpr]
  | Seq     Stmt Stmt
  deriving (Eq,Ord)
  
data AExpr
  = AName Name
  | AConst Integer
  | Add AExpr AExpr
  | Sub AExpr AExpr
  | Mul AExpr AExpr
  | Div AExpr AExpr
  | Neg AExpr
  deriving (Eq,Ord)

data BExpr
  = BConst Bool
  | Lt  AExpr AExpr
  | Lte AExpr AExpr
  | Gt  AExpr AExpr
  | Gte AExpr AExpr
  | Eq  AExpr AExpr
  | Neq AExpr AExpr
  | Not BExpr
  deriving (Eq,Ord)
  
type Name   = String
type Label  = Integer
  
-- * Statement constructors with empty labels

skip     = Skip 0
assign   = Assign 0
ifThen b = IfThen (BExpr 0 b)
while  b = While (BExpr 0 b)
call     = Call 0

-- * Printing programs

instance Show Prog where
  show = show . pp
  
instance PP Prog where
  pp (Prog decls stmts) =
    pp stmts >-< vlist (map pp decls)
  
-- * Printing declarations

instance Show Decl where
  show = show . pp 
  
instance PP Decl where
  pp (Decl n xs body) =
    text n >|< (pp_parens_list 80 $ map pp xs) >#<
      text "{" >-< indent 2 body >-< text "}"
  
-- * Printing statements
  
instance Show Stmt where
  show = show . pp
  
instance PP Stmt where
  pp (Skip _)         = text "skip" >|< text ";"
  pp (Assign _ "return" a)  
                      = text "return" >#< pp a >|< text ";"
  pp (Assign _ n ae)  = text n >#< text "=" >#< pp ae >|< text ";"
  pp (BExpr _ b)      = pp b
  pp (IfThen b t (Skip _))
                      = text "if" >#< pp_parens (pp b) >#<
                          text "{" >-< indent 2 t >-< text "}"
  pp (IfThen b t f)
                      = text "if" >#< pp_parens (pp b) >#<
                          text "{" >-< indent 2 t >-< text "}"
                          >#< "else" >#<
                          text "{" >-< indent 2 f >-< text "}"
  pp (While b l)
                      = text "while" >#< pp_parens (pp b) >#<
                          text "{" >-< indent 2 l >-< text "}"
  pp (Call _ f xs)    = text f >|< (pp_parens_list 80 $ map pp xs) >|< text ";"
  pp (Seq c@(Call _ _ _) (Assign _ n (AName "return")))
                      = text n >#< text "=" >#< pp c
  pp (Seq a b)        = pp a >-< pp b
  
-- * Printing arithmetic expressions
  
instance Show AExpr where
  show = show . pp
  
instance PP AExpr where
  pp (AName n)  = text n
  pp (AConst i) = text (show i)
  pp (Add x y)  = wrap x >#< text "+" >#< wrap y
  pp (Sub x y)  = wrap x >#< text "-" >#< wrap y
  pp (Mul x y)  = wrap x >#< text "*" >#< wrap y
  pp (Div x y)  = wrap x >#< text "/" >#< wrap y
  pp (Neg x)    = text "-" >#< wrap x
  
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
