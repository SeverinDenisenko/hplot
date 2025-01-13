import Expression
import System.Exit
import Data.Text

testEpr :: String -> Double -> IO ()
testEpr expr res = do
    let parsed = parseExpression (pack expr :: Text)
    let evaluated = evalExpression parsed
    if evaluated /= res then
        die (expr ++ " should be equal to " ++ show res ++ " but its " ++ show evaluated)
    else
        print "Test passed."

testFunc :: String -> [(String,Double)] -> Double -> IO ()
testFunc expr m res = do
    let parsed = parseExpression (pack expr :: Text)
    let evaluated = evalFunction parsed m
    if evaluated /= res then
        die (expr ++ " should be equal to " ++ show res ++ " but its " ++ show evaluated)
    else
        print "Test passed."

main :: IO ()
main = do
    testEpr "2.0" 2.0
    testEpr "2" 2.0
    testEpr "22" 22
    testEpr "2.0 * 2.0" 4.0
    testEpr "2.0 + 2.0" 4.0
    testEpr "2.0 * 2.0 + (1.0 * 3.5) ^ 2.0 - 2.0" 14.25
    testEpr "(1.0 + 3.2) * 3.8 ^ 3.0 / 3.4" 67.7830588235294
    testFunc "x" [("x", 2.0)] 2.0
    testFunc "x + y" [("x", 2.0), ("y", 4.0)] 6.0
