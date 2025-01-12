{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Expression
  ( Expression (..),
    evalExpression,
    parceExpression,
  )
where

import Control.Monad
import qualified Control.Monad.Combinators.Expr as E
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

infixl 6 :+:

infixl 6 :-:

infixl 7 :*:

infixl 7 :/:

infixl 8 :^:

data Expression a
  = Val a
  | Var String
  | Expression a :+: Expression a
  | Expression a :-: Expression a
  | Expression a :*: Expression a
  | Expression a :/: Expression a
  | Negative (Expression a)
  | Identical (Expression a)
  | Expression a :^: Expression a

evalExpression :: (Integral a, Fractional a) => Expression a -> a
evalExpression (Var _) = 0 -- todo
evalExpression (Val x) = x
evalExpression (Negative x) = -(evalExpression x)
evalExpression (Identical x) = evalExpression x
evalExpression (x :-: y) = evalExpression x - evalExpression y
evalExpression (x :+: y) = evalExpression x + evalExpression y
evalExpression (x :*: y) = evalExpression x * evalExpression y
evalExpression (x :/: y) = evalExpression x / evalExpression y
evalExpression (x :^: y) = evalExpression x ^ evalExpression y

instance (Show a) => Show (Expression a) where
  show (Val x) = show x
  show (Var n) = show n
  show (Negative x) = "-" ++ show x
  show (Identical x) = show x
  show (x :-: y) = show x ++ " - " ++ show y
  show (x :+: y) = show x ++ " + " ++ show y
  show (x :*: y) = show x ++ " * " ++ show y
  show (x :/: y) = show x ++ " / " ++ show y
  show (x :^: y) = show x ++ " ^ " ++ show y

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
double = L.signed sc (lexeme L.float)

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
    [ binary "*" (*),
      binary "/" (/)
    ],
    [ binary "+" (+),
      binary "-" (-)
    ]
  ]

binary :: Text -> (ExprD -> ExprD -> ExprD) -> E.Operator Parser ExprD
binary name f = E.InfixL (f <$ symbol name)

prefix, postfix :: Text -> (ExprD -> ExprD) -> E.Operator Parser ExprD
prefix name f = E.Prefix (f <$ symbol name)
postfix name f = E.Postfix (f <$ symbol name)

parceExpression :: (Read a, Bounded a) => String -> Expression a
parceExpression str = do
  Val (read str) :+: Val maxBound
