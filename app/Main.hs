module Main where

import Lib

main :: IO ()
main = do
    let expr = parseExpr "if if 0 then true else false then 0 else succ 0"
    print expr
    return ()
