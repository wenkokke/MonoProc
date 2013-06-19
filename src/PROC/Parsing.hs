{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
module PROC.Parsing where

import PROC.Base

import Data.Either (lefts,rights)

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.Idioms
import Text.ParserCombinators.UU.BasicInstances

parseProg  = runParser "stdin" pProg
parseDecl  = runParser "stdin" pDecl
parseStmt  = runParser "stdin" pStmt
parseAExpr = runParser "stdin" pAExpr
parseBExpr = runParser "stdin" pBExpr

pProg :: Parser Prog
pProg = mkProg <$> pMany (pEither pDecl pStmt)
  where
  mkProg xs = Prog (lefts xs) (rights xs)

pDecl :: Parser Decl
pDecl = iI Decl pName (pParens $ pListSep pComma pName) pBlock Ii

pStmt :: Parser Stmt
pStmt = pSkip <|> pAssign <|> pIfThen <|> pWhile <|> pCall <|> pReturn
  where
  pSkip   = Skip <$ iI "skip" ";" Ii
  pAssign = iI Assign pName "=" pAExpr ";" Ii
  pIfThen = iI IfThen "if" pBExpr pBlock pElse Ii
    where
    pElse   :: Parser [Stmt]
    pElse   = iI "else" pBlock Ii `opt` [Skip]
  pWhile  = iI While "while" pBExpr pBlock Ii
  pCall   = iI Call pName (pParens $ pListSep pComma pAExpr) ";" Ii
  pReturn = Assign "return" <$> iI "return" pAExpr ";" Ii
  
pBlock :: Parser [Stmt]
pBlock = iI '{' (pSome pStmt) '}' Ii <?> "Block"
  
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

pName :: Parser Name
pName = lexeme $ (:) <$> pLower <*> pMany (pLetter <|> pDigit)

-- |Flipped function application for operator parsing.
(&) :: a -> (a -> b) -> b
(&) = flip ($)

-- |Maps a list of operators with the same priority to a parser.
samePrio :: [(Name,a)] -> Parser a
samePrio ops = foldr (<|>) empty [ p <$ pSymbol op | (op, p) <- ops ]
