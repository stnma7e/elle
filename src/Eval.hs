module Eval
( EvalState
, StackState
, defaultStack
, reduce
) where

import Data.List
import Control.Monad.State.Lazy
import Debug.Trace

import Syntax (Expr(..), Name)

type EvalState = State StackState

data StackState = StackState
    { stack :: [Name]
    , free :: [Name]
    } deriving (Show)

defaultStack :: StackState
defaultStack = StackState
    { stack = []
    , free = []
    }

reduce :: Expr -> EvalState Expr
reduce app@(App lam@(Lam name fun) expr) = do
    reducedExpr <- reduce expr
    reducedLam <- reduce lam
    lamFreeVar <- findFree reducedLam []
    exprFreeVar <- findFree expr []
    case reducedExpr of
        (Var other) -> if elem other lamFreeVar
            then return app
            else subs (Var name) reducedExpr fun
        otherwise -> if null $ intersect lamFreeVar exprFreeVar
            then subs (Var name) reducedExpr reducedLam
            else return $ App reducedLam reducedExpr
reduce (Lam name expr) = Lam name <$> reduce expr
reduce (If ex1 ex2 ex3) = If <$> reduce ex1 <*> reduce ex2 <*> reduce ex3
reduce x = return x

findFree :: Expr -> [Name] -> EvalState [Name]
findFree (App e1 e2) free = do
    free <- findFree e1 free
    free <- findFree e2 free
    return free
findFree (Var name) free = do
    state <- get
    if elem name $ stack state
        then return free
        else return $ name : free
findFree (Lam name expr) free = do
    initState <- get
    put $ initState { stack = name : stack initState }
    free <- findFree expr free
    finalState <- get
    put $ finalState { stack = stack initState }
    return free
findFree _ free = return free

subs :: Expr -> Expr -> Expr -> EvalState Expr
subs bound@(Var name) with for =
    case for of
        Var other ->
            return $ if name == other
                then with
                else for

        App expr1 expr2 -> do
            ex11 <- subs bound with expr1
            ex22 <- subs bound with expr2
            return $ App ex11 ex22

        Lam lamName expr -> do
            ex11 <- subs bound with expr
            return $ Lam lamName ex11

        If expr1 expr2 expr3 -> do
            expr11 <- subs bound with expr1
            expr22 <- subs bound with expr2
            expr33 <- subs bound with expr3
            return $ If expr11 expr22 expr33

        otherwise -> return for

subs name with for = return for
