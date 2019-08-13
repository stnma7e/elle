module Lib
( parseExpr
, reduce
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Data.Functor.Identity

import Syntax

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
    { Tok.commentStart    = "{-"
    , Tok.commentEnd      = "-}"
    , Tok.commentLine     = "--"
    , Tok.nestedComments  = True
    , Tok.identStart      = letter
    , Tok.identLetter     = alphaNum <|> oneOf "_'"
    , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedNames   = ["true", "false", "if", "then", "else"]
    , Tok.reservedOpNames = ["\\", "."]
    , Tok.caseSensitive   = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

number :: Parser (Either Integer Double)
number = Tok.naturalOrFloat lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)


-- if/then/else
ifthen :: Parser Expr
ifthen = do
    reserved "if"
    cond <- expr
    reservedOp "then"
    tr <- expr
    reserved "else"
    fl <- expr
    return (If cond tr fl)

-- Constants
true, false :: Parser Expr
true  = reserved "true"  >> (return $ Lit (LBool True))
false = reserved "false" >> (return $ Lit (LBool False))

constNum :: Parser Expr
constNum = do
    n <- number
    return . Lit $ case n of
        Left n -> LInt n
        Right n -> LFloat n

var :: Parser Expr
var = identifier >>= (return . Var)

lam :: Parser Expr
lam = do
    reservedOp "\\"
    (Var name) <- var
    reservedOp "."
    ex <- expr
    return (Lam name ex)

app :: Parser Expr
app = do
    fun <- (parens lam <|> lam)
    target <- expr
    return (App fun target)

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

expr :: Parser Expr
expr = Ex.buildExpressionParser [] factor

factor :: Parser Expr
factor = parens expr
    <|> true
    <|> false
    <|> constNum
    <|> app
    <|> lam
    <|> var
    <|> ifthen

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

toplevel :: Parser [Expr]
toplevel = semiSep expr

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<test>" s
