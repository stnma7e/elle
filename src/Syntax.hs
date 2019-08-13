module Syntax where

type Name = String

data Expr
    = Var Name
    | App Expr Expr
    | Lam Name Expr
    | Lit Lit
    | If Expr Expr Expr
    deriving (Eq, Show)

data Lit
    = LInt Integer
    | LBool Bool
    | LFloat Double
    deriving (Eq, Show)
