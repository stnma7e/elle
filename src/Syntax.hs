module Syntax where

type Name = String

data Expr
    = Var Name
    | App Expr Expr
    | Lam Name Expr
    | If Expr Expr Expr
    | Lit Lit
    deriving (Eq, Show)

data Lit
    = LInt Integer
    | LBool Bool
    | LFloat Double
    deriving (Eq, Show)
