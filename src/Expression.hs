{-# LANGUAGE OverloadedStrings #-}

module Expression
  ( Expression (..),
    evalExpression,
    parseExpression,
    evalFunction,
  )
where

import Control.Exception (throw)
import Control.Exception.Base (Exception)
import qualified Control.Monad.Combinators.Expr as E
import Data.Data (Typeable)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Expression a
  = ValueReal a
  | ValueInteger Integer
  | Variable String
  | Add (Expression a) (Expression a)
  | Subtract (Expression a) (Expression a)
  | Multyply (Expression a) (Expression a)
  | Divide (Expression a) (Expression a)
  | Negative (Expression a)
  | Identical (Expression a)
  | Power (Expression a) (Expression a)

evalExpression :: (Floating a) =>Expression a -> a
evalExpression (Variable _) = 0 -- todo
evalExpression (ValueReal x) = x
evalExpression (ValueInteger x) = fromInteger x
evalExpression (Negative x) = -(evalExpression x)
evalExpression (Identical x) = evalExpression x
evalExpression (Add x y) = evalExpression x + evalExpression y
evalExpression (Subtract x y) = evalExpression x - evalExpression y
evalExpression (Multyply x y) = evalExpression x * evalExpression y
evalExpression (Divide x y) = evalExpression x / evalExpression y
evalExpression (Power x y) = evalExpression x ** evalExpression y

data EvaluationExceptionArgumentNotFound = EvaluationExceptionArgumentNotFound deriving (Show)

instance Exception EvaluationExceptionArgumentNotFound

getVariable :: String -> [(String, a)] -> a
getVariable n m = do
  let filtered = filter (\(x, _) -> x == n) m
  if null filtered
    then
      throw EvaluationExceptionArgumentNotFound
    else
      snd (head filtered)

evalFunction :: (Floating a) => Expression a -> [(String, a)] -> a
evalFunction (Variable n) m = getVariable n m
evalFunction (ValueReal x) _ = x
evalFunction (ValueInteger x) _ = fromInteger x
evalFunction (Negative x) m = -(evalFunction x m)
evalFunction (Identical x) m = evalFunction x m
evalFunction (Add x y) m = evalFunction x m + evalFunction y m
evalFunction (Subtract x y) m = evalFunction x m - evalFunction y m
evalFunction (Multyply x y) m = evalFunction x m * evalFunction y m
evalFunction (Divide x y) m = evalFunction x m / evalFunction y m
evalFunction (Power x y) m = evalFunction x m ** evalFunction y m

instance (Show a) => Show (Expression a) where
  show (ValueReal x) = show x
  show (ValueInteger x) = show x
  show (Variable n) = n
  show (Negative x) = "-" ++ show x
  show (Identical x) = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Subtract x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Multyply x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Divide x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Power x y) = "(" ++ show x ++ " ^ " ++ show y ++ ")"

type ExprDouble = Expression Double

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

integer :: Parser Integer
integer = lexeme L.decimal

pVariable :: Parser ExprDouble
pVariable =
  Variable
    <$> lexeme
      ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pReal :: Parser ExprDouble
pReal = ValueReal <$> lexeme double

pInteger :: Parser ExprDouble
pInteger = ValueInteger <$> lexeme integer

pValue :: Parser ExprDouble
pValue = try pReal <|> pInteger

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser ExprDouble
pTerm =
  choice
    [ parens pExpr,
      pVariable,
      pValue
    ]

pExpr :: Parser ExprDouble
pExpr = E.makeExprParser pTerm operatorTable

operatorTable :: [[E.Operator Parser ExprDouble]]
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

binary :: Text -> (ExprDouble -> ExprDouble -> ExprDouble) -> E.Operator Parser ExprDouble
binary name f = E.InfixL (f <$ symbol name)

prefix, postfix :: Text -> (ExprDouble -> ExprDouble) -> E.Operator Parser ExprDouble
prefix name f = E.Prefix (f <$ symbol name)
postfix name f = E.Postfix (f <$ symbol name)

newtype ParseException = ParseException String deriving (Eq, Show, Typeable)

instance Exception ParseException

parseExpression :: Text -> ExprDouble
parseExpression s = case parse (pExpr <* eof) "" s of
  Left e -> throw e
  Right x -> x
