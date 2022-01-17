{-# LANGUAGE LambdaCase #-}
module ParserLexer where

-- Import

import Control.Applicative

import Parser
import Data

-- parseExprs - !

parseExprs :: Parser Exprs
parseExprs = EExprs [] <$ parseChar 'a'

-- parseExpr

parseExpr :: Parser Expr
parseExpr = Expr <$> parseUnary <*> parseSome (parseAnd parseBinop parseSubExpr)
    <|> (`Expr` []) <$> parseUnary

parseSubExpr :: Parser SubExpr
parseSubExpr = EUnary <$> parseUnary
    <|> EExpr <$> parseExpr

-- parseUnary

parseUnary :: Parser Unary
parseUnary = Unop <$> parseUnop <*> parseUnary
    <|> UPostfix <$> parsePostfix

-- parsePostfix

parsePostfix :: Parser Postfix
parsePostfix = (\p c -> Postfix p (Just c)) <$> parsePrimary <*> parseCallExpr
    <|> (`Postfix` Nothing) <$> parsePrimary

-- parseCallExpr

parseCallExpr :: Parser CallExpr
parseCallExpr =  parseOP *> parseListOfExprs <* parseCP
    <|> const [] <$ parseOP <*> parseCP
    where
        parseListOfExprs = (:) <$> (parseWithSpaces parseExpr
                <* parseWithSpaces (parseChar ',')) <*> parseListOfExprs
            <|> (:[]) <$> parseWithSpaces parseExpr

-- parsePrimary

parsePrimary :: Parser Primary
parsePrimary = Id <$> parseIdentifier
    <|> Lit <$> parseLiteral
    <|> PExprs <$> (parseOP *> parseExprs <* parseCP)

-- parseIdentifier

parseIdentifier :: Parser Identifier
parseIdentifier = (:) <$> parseWithSpaces (parseAnyChar allLetters)
        <*> parseWithSpaces (parseSome (parseAnyChar allAlphaNums))
    <|> (:[]) <$> parseWithSpaces (parseAnyChar allLetters)
    where
        allLetters = ['a' .. 'z'] ++ ['A' .. 'Z']
        allAlphaNums = allLetters ++ ['0' .. '9']

-- parseDecimalConst

parseDecimalConst :: Parser DecimalConst
parseDecimalConst = parseWithSpaces parseInt

-- parseDoubleConst

parseDoubleConst :: Parser DoubleConst
parseDoubleConst = parseWithSpaces parseOnlyDotDouble

-- parseLiteral

parseLiteral :: Parser Literal
parseLiteral = LDouble <$> parseDoubleConst
              <|> LInt <$> parseDecimalConst

-- parseBinop

parseBinop :: Parser Binop
parseBinop = Mul <$ parseWithSpaces (parseChar '*')
    <|> Div <$ parseWithSpaces (parseChar '/')
    <|> Add <$ parseWithSpaces (parseChar '+')
    <|> Sub <$ parseWithSpaces (parseChar '-')
    <|> Gt <$ parseWithSpaces (parseChar '>')
    <|> Lt <$ parseWithSpaces (parseChar '<')
    <|> Eq <$ parseWithSpaces (parseString "==")
    <|> Neq <$ parseWithSpaces (parseString "!=")
    <|> Assign <$ parseWithSpaces (parseChar '=')

-- parseUnop

parseUnop :: Parser Unop
parseUnop = Not <$ parseWithSpaces (parseChar '!')
        <|> Minus <$ parseWithSpaces (parseChar '-')

-- Minor parsers

parseWithSpaces :: Parser a -> Parser a
parseWithSpaces p = p <* parseSomeOut (parseAnyChar " \t")
    <|> p

parseOP :: Parser Char
parseOP = parseWithSpaces (parseChar '(')

parseCP :: Parser Char
parseCP = parseWithSpaces (parseChar ')')

--parseWithMaybeSpaces :: Parser a -> Parser a
--parseWithMaybeSpaces p = p <* parseSomeOut (parseAnyChar " \t")
--    <|> p