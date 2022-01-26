{-# LANGUAGE LambdaCase #-}
module ParseCode where

-- Import

import Control.Applicative

import Parser
import Data

-- parseStmt

parseStmt :: Parser Stmt
parseStmt = parseSome parseKdefs

-- parseKdefs

parseKdefs :: Parser Kdefs
parseKdefs = KDefs <$> (parseTextWithSpaces "def " *> parseDefs <* parseTextWithSpaces ";")
    <|> KExprs <$> (parseExprs <* parseTextWithSpaces ";")

-- parseDefs

parseDefs :: Parser Defs
parseDefs = Defs <$> parsePrototype <*> parseExprs

-- parsePrototype

parsePrototype :: Parser Prototype
parsePrototype = Prototype <$> parseIdentifier <*> parsePrototypeArgs

-- parsePrototypeArgs

parsePrototypeArgs :: Parser PrototypeArgs
parsePrototypeArgs = PrototypeArgs <$> (parseOP *> parseArgs <* parseCP)
                                   <*> parseReturn
        <|> PrototypeArgs [] <$> ((parseOP *>  parseCP) *> parseReturn)
    where
        parseArgs =
            parseSome ((parseOneArg <* parseTextWithSpaces ",") <|> parseOneArg)
        parseOneArg = parseAnd (parseIdentifier <* parseTextWithSpaces ":")
                               parseArgsType
        parseReturn = parseTextWithSpaces ":" *> parseArgsType

-- parseArgsType

parseArgsType :: Parser ArgsType
parseArgsType = Int <$ parseTextWithSpaces "int"
    <|> Double <$ parseTextWithSpaces "double"
    <|> Void <$ parseTextWithSpaces "void"

-- parseExprs

parseExprs :: Parser Exprs
parseExprs = EForExpr <$> parseForExpr
    <|> EIfExpr <$> parseIfExpr
    <|> EWhileExpr <$> parseWhileExpr
    <|> (\e es -> EExpr (e:es))
        <$> parseExpr
        <*> parseSome (parseTextWithSpaces ":" *> parseExpr)
    <|> (\e -> EExpr [e]) <$> parseExpr

-- parseForExpr

parseForExpr :: Parser ForExpr
parseForExpr = ForExpr <$> (parseTextWithSpaces "for " *> parseIdAndExpr "=")
                            <*> (parseComma *> parseIdAndExpr "<" <* parseComma)
                            <*> (parseExpr <* parseTextWithSpaces "in ")
                            <*> parseExprs
    where
        parseIdAndExpr s =
            parseAnd (parseIdentifier <* parseTextWithSpaces s) parseExpr
        parseComma = parseTextWithSpaces ","

-- parseIfExpr

parseIfExpr :: Parser IfExpr
parseIfExpr = createIfExpr
        <$> parseIfThen
        <*> (Just <$> (parseTextWithSpaces "else " *> parseExprs))
    <|> (`createIfExpr` Nothing) <$> parseIfThen
    where
        parseIfThen = (,) <$> (parseTextWithSpaces "if " *> parseExpr)
                          <*> (parseTextWithSpaces "then " *> parseExprs)
        createIfExpr (i, t) e = IfExpr i t e

-- parseWhileExpr

parseWhileExpr :: Parser WhileExpr
parseWhileExpr = WhileExpr <$> (parseTextWithSpaces "while " *> parseExpr)
                           <*> (parseTextWithSpaces "do " *> parseExprs)

-- parseExpr

parseExpr :: Parser Expr
parseExpr = Expr <$> parseUnary <*> parseSome (parseAnd parseBinop parseUnary)
    <|> (`Expr` []) <$> parseUnary

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
                <* parseTextWithSpaces ",") <*> parseListOfExprs
            <|> (:[]) <$> parseWithSpaces parseExpr

-- parsePrimary

parsePrimary :: Parser Primary
parsePrimary = PId <$> parseIdentifier
    <|> PLit <$> parseLiteral
    <|> PExprs <$> (parseOP *> parseExprs <* parseCP)

-- parseIdentifier

parseIdentifier :: Parser Identifier
parseIdentifier = (:) <$> parseAnyChar allLetters
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

parseTextWithSpaces :: String -> Parser String
parseTextWithSpaces s = parseWithSpaces (parseString s)

parseOP :: Parser Char
parseOP = parseWithSpaces (parseChar '(')

parseCP :: Parser Char
parseCP = parseWithSpaces (parseChar ')')