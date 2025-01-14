module Script (main) where

import Control.Exception.Base
import Data.Text (pack)
import System.Environment

import Expression

runCalculation :: String -> [FunctionDouble] -> IO (String, [FunctionDouble])
runCalculation i execution_context = do
  let parsed = parseStatement (pack i)
  case parsed of
    Left expression -> makeEvaluation expression execution_context
    Right function -> makeSubst function execution_context
  where
    makeSubst :: FunctionDouble -> [FunctionDouble] -> IO (String, [FunctionDouble])
    makeSubst function execution_context = do
      let subst = substituteFunctions function execution_context
      return (show subst, subst:execution_context)
    makeEvaluation :: ExpressionDouble -> [FunctionDouble] -> IO (String, [FunctionDouble])
    makeEvaluation expr execution_context = do
      let (Function _ subst) = substituteFunctions (Function "" expr) execution_context
      let calced = evalFunction subst execution_context
      return (show calced, execution_context)

executeCode :: [String] -> [FunctionDouble] -> IO ()
executeCode [] _ = print ("Done." :: String)
executeCode (l1 : ll) execution_context = do
  (result, new_execution_context) <- runCalculation l1 execution_context
  print result
  executeCode ll new_execution_context

executeCodeSafe :: [String] -> IO ()
executeCodeSafe ll = do
  catch (executeCode ll []) handler
  where
    handler :: SomeException -> IO ()
    handler e = print (show e)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then
      print "Please, supply input file"
    else do
      content <- readFile (head args)
      let linesOfCode = lines content
      executeCodeSafe linesOfCode
