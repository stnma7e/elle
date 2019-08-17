module Syntax where

newtype Name = Name String
    deriving (Eq)
instance Show Name where
    show (Name n) = "(Name \"" ++ n ++ "\")"

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
