{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Script (main) where

import Control.Exception (catch)
import Control.Exception.Base
import Expression
import Data.Text(pack)
import System.Environment

runCalculation :: String -> IO String
runCalculation i = do
  let expr = parseExpression (pack i)
  let calced = evalExpression expr
  return (show calced)

executeCode :: [String] -> IO ()
executeCode [] = print ("Done." :: String)
executeCode (l1:ll) = do
  result <- runCalculation l1
  print result
  executeCode ll

executeCodeSafe :: [String] -> IO ()
executeCodeSafe ll = do
  catch (executeCode ll) handler
  where
    handler :: SomeException -> IO ()
    handler e = print (show e)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let linesOfCode = lines content
  executeCodeSafe linesOfCode
