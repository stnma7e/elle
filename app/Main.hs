module Main where

import Data.Either
import Control.Monad.State.Lazy

import Lib (reduce, defaultStack)
import Parser (parseExpr)

main :: IO ()
main = do
    let (Right expr) = parseExpr "\
\if if 1000.50 then true else false then 0 else \\x . (if x then y else z) y \
    \"
    print expr
    putStrLn ""
    print $ runState (reduce expr) defaultStack
    return ()
