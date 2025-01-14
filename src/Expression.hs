{-# LANGUAGE OverloadedStrings #-}

module Expression
  ( Expression (..),
    Function (..),
    evalExpression,
    evalFunction,
    parseExpression,
    parseFunction,
    parseStatement,
    substituteFunctions,
    FunctionDouble,
    ExpressionDouble
  )
where

import Control.Exception (throw)
import Control.Exception.Base (Exception)
import qualified Control.Monad.Combinators.Expr as E
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

data EvaluationExceptionArgumentNotFound = EvaluationExceptionArgumentNotFound deriving (Show)

instance Exception EvaluationExceptionArgumentNotFound

data EvaluationExceptionArgumentsAreNotAllowed = EvaluationExceptionArgumentsAreNotAllowed deriving (Show)

instance Exception EvaluationExceptionArgumentsAreNotAllowed

evalExpression :: (Floating a) => Expression a -> a
evalExpression (Variable _) = throw EvaluationExceptionArgumentsAreNotAllowed
evalExpression (ValueReal x) = x
evalExpression (ValueInteger x) = fromInteger x
evalExpression (Negative x) = -(evalExpression x)
evalExpression (Identical x) = evalExpression x
evalExpression (Add x y) = evalExpression x + evalExpression y
evalExpression (Subtract x y) = evalExpression x - evalExpression y
evalExpression (Multyply x y) = evalExpression x * evalExpression y
evalExpression (Divide x y) = evalExpression x / evalExpression y
evalExpression (Power x y) = evalExpression x ** evalExpression y

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

type ExpressionDouble = Expression Double

-- Parsing Expressions

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

pVariable :: Parser ExpressionDouble
pVariable =
  Variable
    <$> lexeme
      ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pReal :: Parser ExpressionDouble
pReal = ValueReal <$> lexeme double

pInteger :: Parser ExpressionDouble
pInteger = ValueInteger <$> lexeme integer

pValue :: Parser ExpressionDouble
pValue = try pReal <|> pInteger

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser ExpressionDouble
pTerm =
  choice
    [ parens pExpr,
      pVariable,
      pValue
    ]

pExpr :: Parser ExpressionDouble
pExpr = E.makeExprParser pTerm operatorTable

operatorTable :: [[E.Operator Parser ExpressionDouble]]
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

binary :: Text -> (ExpressionDouble -> ExpressionDouble -> ExpressionDouble) -> E.Operator Parser ExpressionDouble
binary name f = E.InfixL (f <$ symbol name)

prefix, postfix :: Text -> (ExpressionDouble -> ExpressionDouble) -> E.Operator Parser ExpressionDouble
prefix name f = E.Prefix (f <$ symbol name)
postfix name f = E.Postfix (f <$ symbol name)

parseExpression :: Text -> ExpressionDouble
parseExpression s = case parse (pExpr <* eof) "" s of
  Left e -> throw e
  Right x -> x

-- Functions

data Function a = Function String (Expression a) deriving Show

substituteExpressions :: Expression a -> Expression a -> String -> Expression a
substituteExpressions (Variable v) sub_expr sub_name
  | v == sub_name = sub_expr
  | otherwise = Variable v
substituteExpressions x _ _ = x

substituteFunctions :: Function a -> [Function a] -> Function a
substituteFunctions f [] = f
substituteFunctions (Function name expr) ((Function sub_name sub_expr) : ss) = do
  let substituted = substituteExpressions expr sub_expr sub_name
  substituteFunctions (Function name substituted) ss

type FunctionDouble = Function Double

getFunction :: String -> [Function a] -> Function a
getFunction n m = do
  let filtered = filter (\(Function k _) -> k == n) m
  if null filtered
    then
      throw EvaluationExceptionArgumentNotFound
    else
      head filtered

evalFunction :: (Floating a) => Expression a -> [Function a] -> a
evalFunction (Variable n) m = do
  let (Function _ e) = getFunction n m
  evalFunction e m
evalFunction (ValueReal x) _ = x
evalFunction (ValueInteger x) _ = fromInteger x
evalFunction (Negative x) m = -(evalFunction x m)
evalFunction (Identical x) m = evalFunction x m
evalFunction (Add x y) m = evalFunction x m + evalFunction y m
evalFunction (Subtract x y) m = evalFunction x m - evalFunction y m
evalFunction (Multyply x y) m = evalFunction x m * evalFunction y m
evalFunction (Divide x y) m = evalFunction x m / evalFunction y m
evalFunction (Power x y) m = evalFunction x m ** evalFunction y m

-- Parsing functions

pString :: Parser String
pString =
  lexeme
    ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pFunction :: Parser FunctionDouble
pFunction = do
  name <- pString
  _ <- sc
  _ <- char '='
  _ <- sc
  Function name <$> pExpr

parseFunction :: Text -> FunctionDouble
parseFunction s = case parse (pFunction <* eof) "" s of
  Left e -> throw e
  Right x -> x

pStatement :: Parser (Either ExpressionDouble FunctionDouble)
pStatement = try pFunctionW <|> pExprW
  where
    pExprW :: Parser (Either ExpressionDouble FunctionDouble)
    pExprW = Left <$> pExpr
    pFunctionW :: Parser (Either ExpressionDouble FunctionDouble)
    pFunctionW = Right <$> pFunction

parseStatement :: Text -> Either ExpressionDouble FunctionDouble
parseStatement s = case parse (pStatement <* eof) "" s of
  Left e -> throw e
  Right x -> x
