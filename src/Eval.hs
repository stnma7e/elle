module Eval
( EvalState
, StackState
, defaultStack
, reduce
) where

import Data.List
import Control.Monad.State.Lazy

import Syntax (Expr(..), Name)

type EvalState = State StackState

data StackState = StackState
    { stack :: [Name]
    } deriving (Show)

defaultStack :: StackState
defaultStack = StackState
    { stack = []
    }

reduce :: Expr -> EvalState Expr
reduce (App (Lam bound lamExpr) expr) = do
    reducedExpr <- reduce expr
    reducedLamExpr <- reduce lamExpr
    let reducedLam = (Lam bound reducedLamExpr)
    lamFreeVar <- findFree reducedLam []
    exprFreeVar <- findFree reducedExpr []
    if null $ intersect lamFreeVar exprFreeVar
            then reduce $ subs (Var bound) reducedExpr reducedLamExpr
            else return $ App reducedLam reducedExpr

reduce (App ex1 ex2) = do
    reducedEx1 <- reduce ex1
    reducedEx2 <- reduce ex2
    case reducedEx1 of
        lam@(Lam _ _) -> reduce $ App lam reducedEx2
        otherwise -> return $ App reducedEx1 reducedEx2

reduce (Lam name expr) = do
    push name
    reducedExpr <- reduce expr
    pop
    return $ Lam name reducedExpr

reduce (If ex1 ex2 ex3) = do
    reducedEx1 <- reduce ex1
    reducedEx2 <- reduce ex2
    reducedEx3 <- reduce ex3
    return $ If reducedEx1 reducedEx2 reducedEx3

reduce x = return x

push :: Name -> EvalState ()
push name = do
    state <- get
    put $ state { stack = name : stack state }

pop :: EvalState ()
pop = do
    state <- get
    if null $ stack state
        then return ()
        else put $ state { stack = init $ stack state }

findFree :: Expr -> [Name] -> EvalState [Name]
findFree (Var name) free = do
    state <- get
    if elem name $ stack state
        then return free
        else return $ name : free

findFree (Lam name expr) free = do
    push name
    free <- findFree expr free
    pop
    return free

findFree (App e1 e2) free = findFree e1 free >>= findFree e2

findFree _ free = return free

subs :: Expr -> Expr -> Expr -> Expr
subs bound@(Var name) with for =
    case for of
        Var other ->
            if name == other
                then with
                else for

        App expr1 expr2 ->
            let ex11 = subs bound with expr1
                ex22 = subs bound with expr2
            in App ex11 ex22

        Lam name expr ->
            let ex11 = subs bound with expr
            in Lam name ex11

        If expr1 expr2 expr3 ->
            let expr11 = subs bound with expr1
                expr22 = subs bound with expr2
                expr33 = subs bound with expr3
            in If expr11 expr22 expr33

subs _ _ for = undefined
