{-# LANGUAGE FlexibleInstances #-}

module Pretty where

import Prelude hiding ((<>))
import Text.PrettyPrint

import Syntax

class Pretty p where
    ppr :: Int -> p -> Doc
    pp :: p -> Doc
    pp = ppr 0

instance Pretty String where
    ppr _ x = text x

instance Pretty Expr where
  ppr p e = case e of
    Lit (LInt a)  -> text (show a)
    Lit (LBool b) -> text (show b)
    Var (Name x)   -> text x
    App a b -> parensIf (p > 0) $ (ppr (p + 1) a) <+> (ppr p b)
    Lam x a -> parensIf (p > 0) $
        char '\\'
        <> hsep (fmap pp (viewVars e))
        <+> text "->"
        <+> ppr (p + 1) (viewBody e)

ppexpr :: Expr -> String
ppexpr = render . ppr 0

viewVars :: Expr -> [String]
viewVars (Lam (Name n) a) = n : viewVars a
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (Lam _ a) = viewBody a
viewBody x = x

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id
