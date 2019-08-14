module Main where

import Control.Monad.State.Lazy

import Eval (reduce, defaultStack)
import Parser (parseExpr)

main :: IO ()
main = do
    let src =
            "if if 1000.50 then true else false then 0 else (\\x . (if x then y else z)) (\\c.b)"
            -- "(\\x . x z (y z)) (\\q. b x)"
    print $ parseExpr src
    let (Right expr) = parseExpr src
    putStrLn ""
    print $ runState (reduce expr) defaultStack
