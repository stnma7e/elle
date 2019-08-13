module Main where

import Lib (reduce)
import Lexer (parseExpr)

main :: IO ()
main = do
    let expr = parseExpr "\
\if if 1000.50 then true else false then 0 else \\x . (if x then y else z) y \
    \"
    print expr
    putStrLn ""
    print $ fmap reduce expr
    return ()
