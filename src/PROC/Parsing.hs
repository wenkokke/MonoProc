{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module PROC.Parsing where

import PROC.Base

import Data.Either (lefts,rights)

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.Idioms
import Text.ParserCombinators.UU.BasicInstances

-- |Parser for programs.
pProg :: Parser Prog
pProg = mkProg <$> pMany (pEither pDecl pStmt)
  where
  mkProg xs = Prog (lefts xs) (foldr1 Seq (rights xs))

-- |Parser for declarations.
pDecl :: Parser Decl
pDecl = iI Decl pName (pParens $ pListSep pComma pName) pBlock Ii <?> "Declaration"

-- |Parser for statements.
pStmt :: Parser Stmt
pStmt = pSkip <|> pIfThen <|> pWhile <|> pCall <|> pReturn <|> pAssign <|> pCallAssign <?> "Statement"
  where
  pSkip       = skip <$ iI "skip" ";" Ii
  pAssign     = iI assign pName "=" pAExpr ";" Ii
  pIfThen     = iI ifThen "if" (pParens pBExpr) pBlock pElse Ii
    where
    pElse     :: Parser Stmt
    pElse     = iI "else" pBlock Ii <<|> pure skip
  pWhile      = iI while "while" pBExpr pBlock Ii
  pCall       = iI call pName (pParens $ pListSep pComma pAExpr) ";" Ii
  pCallAssign = iI callAssign pName "=" pCall Ii
    where
    callAssign n c = Seq c (assign n (AName "return"));
  pReturn     = assign "return" <$> iI "return" pAExpr ";" Ii

-- |Parser for block expressions.
pBlock :: Parser Stmt
pBlock = iI '{' (foldr1 Seq <$> pSome pStmt) '}' Ii <?> "Block"
  
-- |Parser for arithmetic expressions.
pAExpr :: Parser AExpr
pAExpr = pOper <?> "AExpr"
  where
  pAtom :: Parser AExpr
  pAtom = AName <$> pName
      <|> AConst <$> pNatural
      <|> iI Neg '-' pAtom Ii
      <|> pParens pAExpr
  
  pOper :: Parser AExpr
  pOper = foldr pChainl pAtom (map samePrio aOper)
  aOper :: [[(Name,AExpr -> AExpr -> AExpr)]]
  aOper = [[("+",Add),("-",Sub)],[("*",Mul),("/",Div)]]

-- |Parser for boolean expressions.
pBExpr :: Parser BExpr
pBExpr = pAtom <|> pOper <?> "BExpr"
  where
  pAtom :: Parser BExpr
  pAtom = BConst True  <$ pSymbol "true"
      <|> BConst False <$ pSymbol "false"
      <|> iI Not '~' pAtom Ii
      <|> pParens pBExpr
      
  pOper :: Parser BExpr
  pOper = iI (&) pAExpr (samePrio bOper) pAExpr Ii
  bOper :: [(Name,AExpr -> AExpr -> BExpr)]
  bOper = [("<",Lt),("<=",Lte),(">",Gt),(">=",Gte),("==",Eq),("!=",Neq)]

-- |Parser for names.
pName :: Parser Name
pName = lexeme $ (:) <$> pLower <*> pMany (pLetter <|> pDigit <|> pUnderscore)

-- |Parser for several other characters.
pUnderscore :: Parser Char
pUnderscore = pSym '_'

-- |Flipped function application for operator parsing.
(&) :: a -> (a -> b) -> b
(&) = flip ($)

-- |Maps a list of operators with the same priority to a parser.
samePrio :: [(Name,a)] -> Parser a
samePrio ops = foldr (<|>) empty [ p <$ pSymbol op | (op, p) <- ops ]
