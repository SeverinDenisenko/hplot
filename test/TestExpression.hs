module TestExpression
  ( testExpression,
  )
where

import Expression
import System.Exit
import Data.Text
import Control.Monad (when)

testEpr :: String -> Double -> IO ()
testEpr expr res = do
    let parsed = parseExpression (pack expr :: Text)
    let evaluated = evalExpression parsed
    when (evaluated /= res) $ die (expr ++ " should be equal to " ++ show res ++ " but its " ++ show evaluated)

testFunc :: String -> [(String,Double)] -> Double -> IO ()
testFunc expr m res = do
    let parsed = parseExpression (pack expr :: Text)
    let evaluated = evalFunction parsed m
    when (evaluated /= res) $ die (expr ++ " should be equal to " ++ show res ++ " but its " ++ show evaluated)

testExpression :: IO ()
testExpression = do
    testEpr "2.0" 2.0
    testEpr "2" 2.0
    testEpr "2e1" 20.0
    testEpr "22" 22
    testEpr "2.0 * 2.0" 4.0
    testEpr "2 + 2.0" 4.0
    testEpr "2.0 * 2 + (1.0 * 3.5) ^ 2.0 - 2" 14.25
    testEpr "(1e0 + 3.2) * 3.8 ^ 3.0 / 3.4" 67.7830588235294
    testFunc "x" [("x", 2.0)] 2.0
    testFunc "x + y" [("x", 2.0), ("y", 4.0)] 6.0
