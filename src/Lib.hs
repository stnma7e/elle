module Lib
( reduce
, defaultStack
) where

import Control.Monad.State.Lazy

import Syntax

data Value =
    Value Expr

data StackState = StackState
    { stack :: [Name]
    } deriving (Show)

type EvalState = State StackState

defaultStack :: StackState
defaultStack = StackState
    { stack = []
    }

eval :: Expr -> EvalState Value
eval = undefined

reduce :: Expr -> EvalState Expr
reduce (App (Lam name lamExp) appExp) = do
    ex0 <- reduce appExp
    subs (Var name) ex0 lamExp
reduce (If ex1 ex2 ex3) = do
    ex11 <- reduce ex1
    ex22 <- reduce ex2
    ex33 <- reduce ex3
    return $ If ex11 ex22 ex33
reduce x = return x

subs :: Expr -> Expr -> Expr -> EvalState Expr
subs (Var name1) with var2@(Var name2) =
    return $ if name1 == name2
        then with
        else var2
subs name@(Var _) with (App ex1 ex2) = do
    ex11 <- subs name with ex1
    ex22 <- subs name with ex2
    return $ App ex11 ex22
subs varName@(Var _) with (Lam lamName ex1) = do
    ex11 <- subs varName with ex1
    -- first check that `lamName` is not one of the free variables of `with`
    return $ Lam lamName ex11
subs name@(Var _) with (If ex1 ex2 ex3) = do
    ex11 <- subs name with ex1
    ex22 <- subs name with ex2
    ex33 <- subs name with ex3
    return $ If ex11 ex22 ex33
subs name with for = return for
