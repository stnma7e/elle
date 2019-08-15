module Main where

import Control.Monad.State.Lazy

import Eval (reduce, defaultStack)
import Parser (parseExpr)

main :: IO ()
main = do
    let src =
            -- SKK identity
            "(\\x y z. x z (y z)) (\\x y. x) (\\x y . x)"
    print $ parseExpr src
    let (Right expr) = parseExpr src
    putStrLn ""
    let state = runState (reduce expr) defaultStack
    print state
