module Parser
( parseExpr
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

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
    , Tok.reservedNames   = ["true", "false", "if", "then", "else", "let", "in"]
    , Tok.reservedOpNames = ["\\", ".", ":"]
    , Tok.caseSensitive   = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

number :: Parser (Either Integer Double)
number = Tok.naturalOrFloat lexer

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
true  = reserved "true"  >> return (Lit (LBool True))
false = reserved "false" >> return (Lit (LBool False))
constNum = do
    n <- number
    return . Lit $ case n of
        Left n -> LInt n
        Right n -> LFloat n

var :: Parser Expr
var = (Var . Name) <$> identifier

lam :: Parser Expr
lam = do
    reservedOp "\\"
    names <- many1 var
    reservedOp "."
    ex <- expr
    let (Var firstName) = last names
    return $ foldr
        (\(Var name) acc -> Lam name acc)
        (Lam firstName ex)
        (init names)

letin :: Parser Expr
letin = do
    reserved "let"
    (Var name) <- var
    reservedOp "="
    binding <- expr
    reserved "in"
    ex <- expr
    return $ App (Lam name ex) binding

expr :: Parser Expr
expr = foldl1 App <$> many1 term

term :: Parser Expr
term = parens expr
    <|> ifthen
    <|> true
    <|> false
    <|> constNum
    <|> var
    <|> letin
    <|> lam

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<test>"
