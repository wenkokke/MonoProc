{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module PROC.Parsing where

import PROC.Base
import PROC.MF.Labelable (runLabel)
import PROC.MF.Analysis.RD (RD (..))
import PROC.MF.Analysis.CP (CP (..), ZT (..))

import Data.Either (lefts,rights)
import Data.Set (Set)
import qualified Data.Set as S (fromList)
import Data.Map (Map)
import qualified Data.Map as M (fromList)

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.Idioms
import Text.ParserCombinators.UU.BasicInstances

-- |Parses a program by running the @pProg@ parser.
parseProg  = runLabel . runParser "stdin" pProg

-- |Parses a declaration expression by running the @pDecl@ parser.
parseDecl  = runLabel . runParser "stdin" pDecl

-- |Parses a statement expression by running the @pStmt@ parser.
parseStmt  = runLabel . runParser "stdin" pStmt

-- |Parses an arithmetic expression by running the @pAExpr@ parser.
parseAExpr = runParser "stdin" pAExpr

-- |Parses a boolean expression by running the @pBExpr@ parser.
parseBExpr = runParser "stdin" pBExpr

-- * Parsing the MonoProc Language

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
  pWhile      = iI while "while" (pParens pBExpr) pBlock Ii
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

-- * Parsing the Analyses' Results

-- |Sets of @Name@s are used as the result type of e.g. Live Variable Analysis.
pNameSet :: Parser (Set Name)
pNameSet = pSetOf pName

-- |Sets of @AExpr@s are used as the result type of e.g. Available Expression Analysis.
pAExprSet :: Parser (Set AExpr)
pAExprSet = pSetOf pAExpr

-- |Sets of @RD@s are used as the result type of Reached-Definition Analysis.
pRDSet :: Parser (Set RD)
pRDSet = pSetOf pRD

-- |Parses Reached-Definition information.
pRD :: Parser RD
pRD = iI RD "(" pName "," (pNone <|> pSome) ")" Ii
  where
  pNone = Nothing <$ pSym '?'
  pSome = Just <$> pNatural

-- |Parses Constant-Propagation information.
pCP :: Parser CP
pCP = pNone <|> (CP <$> pMapOf pSome)
  where
  pSome :: Parser (Name,ZT)
  pSome = iI (,) "(" pName "," pZT ")" Ii
  pNone = Bottom <$ pSymbol "Bottom"

-- |Parses Z+T expressions.
pZT :: Parser ZT
pZT = pSome <|> pNone
  where
  pSome = Z <$> pInteger
  pNone = Top <$ pSym 'T'

-- * Utility Functions and Parsers

-- |Parses a set of value, as @{a,b,c}@.
pSetOf :: (Ord a) => Parser a -> Parser (Set a)
pSetOf p = S.fromList <$> pBraces (pListSep pComma p)

-- |Parses a set of value, as @{a,b,c}@.
pMapOf :: (Ord k) => Parser (k,v) -> Parser (Map k v)
pMapOf p = M.fromList <$> pBraces (pListSep pComma p)

-- |Maps a list of operators with the same priority to a parser.
samePrio :: [(Name,a)] -> Parser a
samePrio ops = foldr (<|>) empty [ p <$ pSymbol op | (op, p) <- ops ]

-- |Parser for several other characters.
pUnderscore :: Parser Char
pUnderscore = pSym '_'

-- |Flipped function application used in operator parsing.
(&) :: a -> (a -> b) -> b
(&) = flip ($)
