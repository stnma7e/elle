module Parser
( parseExpr
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
true, false, constNum :: Parser Expr
true  = reserved "true"  >> (return $ Lit (LBool True))
false = reserved "false" >> (return $ Lit (LBool False))
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

expr :: Parser Expr
expr = do
    es <- many1 term
    return (foldl1 App es)

term :: Parser Expr
term = parens expr
    <|> ifthen
    <|> true
    <|> false
    <|> constNum
    <|> var
    <|> lam

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
