module Main where

import Control.Exception
import Control.Monad.State.Lazy

import Eval (reduce, defaultStack)
import Syntax (Expr(..), Name(..))
import Parser (parseExpr)
import Pretty (ppexpr)

main :: IO (Either SomeException ())
main = try $ evaluate =<< do
    let src =
            -- SKK identity
            "let S = (\\x y z. x z (y z)) \
            \ in let K = (\\x y. x) in"
            ++ " \
            \ let Y = \\f. (\\x. (f (x x)) \\x. (f (x x))) in \
            \ S K K"
            -- \ (\\x . x) 1"
            -- \ let Y1 = S S K (S (K (S S (S (S S K)))) K) in Y1"
            -- \ S K K"

            -- \ let fix = (\\v. v (\\x. fix v) x) in \
            -- \ fix"
            --

            -- ω identity
            -- "(\\x. x x) (\\x. x x)"
    print $ parseExpr src
    expr <- case parseExpr src of
        Right ex -> return ex
        Left e -> print e >> undefined
    -- let expr = Lam (Name "z") (App (App (Lam (Name "z") (Lam (Name "z") (App (Var (Name "z")) (App (App (Var (Name "z")) (App (Lam (Name "y") (Var (Name "z"))) (Lam (Name "y") (Var (Name "z"))))) (App (Lam (Name "y") (Var (Name "z"))) (Lam (Name "y") (Var (Name "z")))))))) (Lam (Name "z") (Lam (Name "z") (App (Var (Name "z")) (App (App (Var (Name "z")) (App (Lam (Name "y") (Var (Name "z"))) (Lam (Name "y") (Var (Name "z"))))) (App (Lam (Name "y") (Var (Name "z"))) (Lam (Name "y") (Var (Name "z"))))))))) (Lam (Name "z") (Lam (Name "z") (App (Var (Name "z")) (App (App (Var (Name "z")) (App (Lam (Name "y") (Var (Name "z"))) (Lam (Name "y") (Var (Name "z"))))) (App (Lam (Name "y") (Var (Name "z"))) (Lam (Name "y") (Var (Name "z")))))))))

    putStrLn ""
    putStrLn $ ppexpr expr
    putStrLn ""

    let (expr1, state) = runState (reduce expr) defaultStack
    print expr1
    putStrLn ""
    putStrLn $ ppexpr expr1
    putStrLn ""
    print state
