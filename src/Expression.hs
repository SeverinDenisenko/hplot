{-# LANGUAGE OverloadedStrings #-}

module Expression
  ( Expression (..),
    Function (..),
    evalFunction,
    parseExpression,
    parseFunction,
    parseStatement,
    substituteFunctions,
    FunctionDouble,
    ExpressionDouble,
    ExpressionError (..),
    ParseException (..),
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
  | InternalFunction String (a -> a) (Expression a)

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
  show (InternalFunction n _ e) = n ++ "(" ++ show e ++ ")"

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

data ParseException = ParseException deriving (Show)

instance Exception ParseException

parseExpression :: Text -> ExpressionDouble
parseExpression s = case parse (pExpr <* eof) "" s of
  Left _ -> throw ParseException
  Right x -> x

-- Functions

data Function a = Function String (Expression a)

instance (Show a) => Show (Function a) where
  show (Function name expr) = name ++ " = " ++ show expr

instance Eq (Function a) where
  (Function n1 _) == (Function n2 _) = n1 == n2

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

data ExpressionError = ExpressionErrorArgumentNotFound | ExpressionErrorAtithmeticError deriving (Show)

getFunction :: String -> [Function a] -> Either (Function a) ExpressionError
getFunction n m = do
  let filtered = filter (\(Function k _) -> k == n) m
  if null filtered
    then
      Right ExpressionErrorArgumentNotFound
    else
      Left (head filtered)

evalFunction :: (Floating a) => Expression a -> [Function a] -> Either a ExpressionError
evalFunction (Variable n) m = case getFunction n m of
  Left (Function _ e) -> evalFunction e m
  Right err -> Right err
evalFunction (ValueReal x) _ = Left x
evalFunction (ValueInteger x) _ = Left (fromInteger x)
evalFunction (Negative x) m = case evalFunction x m of
  Left result -> Left (-result)
  Right err -> Right err
evalFunction (Identical x) m = evalFunction x m
evalFunction (Add x y) m = case (evalFunction x m, evalFunction y m) of
  (Left a, Left b) -> Left (a + b)
  (Right e, Left _) -> Right e
  (Left _, Right e) -> Right e
  (Right e, Right _) -> Right e
evalFunction (Subtract x y) m = case (evalFunction x m, evalFunction y m) of
  (Left a, Left b) -> Left (a - b)
  (Right e, Left _) -> Right e
  (Left _, Right e) -> Right e
  (Right e, Right _) -> Right e
evalFunction (Multyply x y) m = case (evalFunction x m, evalFunction y m) of
  (Left a, Left b) -> Left (a * b)
  (Right e, Left _) -> Right e
  (Left _, Right e) -> Right e
  (Right e, Right _) -> Right e
evalFunction (Divide x y) m = case (evalFunction x m, evalFunction y m) of
  (Left a, Left b) -> Left (a / b)
  (Right e, Left _) -> Right e
  (Left _, Right e) -> Right e
  (Right e, Right _) -> Right e
evalFunction (Power x y) m = case (evalFunction x m, evalFunction y m) of
  (Left a, Left b) -> Left (a ** b)
  (Right e, Left _) -> Right e
  (Left _, Right e) -> Right e
  (Right e, Right _) -> Right e
evalFunction (InternalFunction _ f x) m = case evalFunction x m of
  Left result -> Left (f result)
  Right err -> Right err

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
  Left _ -> throw ParseException
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
  Left _ -> throw ParseException
  Right x -> x
