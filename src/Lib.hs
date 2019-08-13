module Lib
( reduce
) where

import Syntax

reduce :: Expr -> Expr
reduce (App (Lam name lamExp) appExp) =
    let ex0 = reduce appExp
    in subs (Var name) ex0 lamExp
reduce (If ex1 ex2 ex3) =
    If (reduce ex1) (reduce ex2) (reduce ex3)
reduce x = x

subs :: Expr -> Expr -> Expr -> Expr
subs (Var name1) with (Var name2) =
    if name1 == name2
        then with
        else Var name2
subs name@(Var _) with (App ex1 ex2) =
    let ex11 = subs name with ex1
        ex22 = subs name with ex2
    in App ex11 ex22
subs varName@(Var _) with (Lam lamName ex1) =
    let ex11 = subs varName with ex1
    in Lam lamName ex11
subs name@(Var _) with (If ex1 ex2 ex3) =
    let ex11 = subs name with ex1
        ex22 = subs name with ex2
        ex33 = subs name with ex3
    in If ex11 ex22 ex33
subs _ _ for = for
