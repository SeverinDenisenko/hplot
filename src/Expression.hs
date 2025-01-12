{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Expression
  ( Expression (..),
    evalExpression,
    parseExpression,
  )
where

import Control.Exception (throw)
import Control.Exception.Base (Exception)
import Control.Monad
import qualified Control.Monad.Combinators.Expr as E
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Expression a
  = Val a
  | Var String
  | Add (Expression a) (Expression a)
  | Subtract (Expression a) (Expression a)
  | Multyply (Expression a) (Expression a)
  | Divide (Expression a) (Expression a)
  | Negative (Expression a)
  | Identical (Expression a)
  | Power (Expression a) (Expression a)

evalExpression :: (Floating a) => Expression a -> a
evalExpression (Var _) = 0 -- todo
evalExpression (Val x) = x
evalExpression (Negative x) = -(evalExpression x)
evalExpression (Identical x) = evalExpression x
evalExpression (Add x y) = evalExpression x + evalExpression y
evalExpression (Subtract x y) = evalExpression x - evalExpression y
evalExpression (Multyply x y) = evalExpression x * evalExpression y
evalExpression (Divide x y) = evalExpression x / evalExpression y
evalExpression (Power x y) = evalExpression x ** evalExpression y

instance (Show a) => Show (Expression a) where
  show (Val x) = show x
  show (Var n) = show n
  show (Negative x) = "-" ++ show x
  show (Identical x) = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Subtract x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Multyply x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Divide x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Power x y) = "(" ++ show x ++ " ^ " ++ show y ++ ")"

type ExprD = Expression Double

-- Parsing

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

double :: Parser Double
double = lexeme L.float

pVariable :: Parser ExprD
pVariable =
  Var
    <$> lexeme
      ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pDouble :: Parser ExprD
pDouble = Val <$> lexeme double

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser ExprD
pTerm =
  choice
    [ parens pExpr,
      pVariable,
      pDouble
    ]

pExpr :: Parser ExprD
pExpr = E.makeExprParser pTerm operatorTable

operatorTable :: [[E.Operator Parser ExprD]]
operatorTable =
  [ [ prefix "-" Negative,
      prefix "+" Identical
    ],
    [ binary "^" Power
    ],
    [ binary "*" Multyply,
      binary "/" Divide
    ],
    [ binary "+" Add,
      binary "-" Subtract
    ]
  ]

binary :: Text -> (ExprD -> ExprD -> ExprD) -> E.Operator Parser ExprD
binary name f = E.InfixL (f <$ symbol name)

prefix, postfix :: Text -> (ExprD -> ExprD) -> E.Operator Parser ExprD
prefix name f = E.Prefix (f <$ symbol name)
postfix name f = E.Postfix (f <$ symbol name)

data ParseException = ParseException deriving (Show)

instance Exception ParseException

parseExpression :: Text -> ExprD
parseExpression s = case parse (pExpr <* eof) "" s of
  Left _ -> throw ParseException
  Right x -> x
