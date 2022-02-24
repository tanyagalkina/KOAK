{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module ParseCode where

-- Import

import Control.Applicative ( Alternative((<|>)) )

import Parser
    ( parseAnd,
      parseAnyChar,
      parseChar,
      parseEnd,
      parseInt,
      parseOnlyDotDouble,
      parseSome,
      parseSomeOut,
      parseString,
      Parser )
import Data
    ( Unop(..),
      Binop(..),
      Literal(..),
      DoubleConst,
      DecimalConst,
      Identifier,
      Primary(..),
      CallExpr,
      Postfix(..),
      Unary(..),
      Expr(..),
      WhileExpr(..),
      IfExpr(..),
      ForExpr(..),
      Exprs(..),
      ArgsType(..),
      PrototypeArgs(..),
      Prototype(..),
      Defs(..),
      Kdefs(..),
      Stmt,
      Boolean (..), Coms)

-- parseStmt

parseStmt :: Parser Stmt
parseStmt = parseSome parseKdefs <* parseWithSpaces parseEnd

-- parseKdefs

parseKdefs :: Parser Kdefs
parseKdefs = KComs <$> (parseTextWithSpaces "#"
                            *> parseComs <* parseTextWithSpaces "#")
    <|> KDefs <$> (parseTextWithSpacesAfter "def"
                            *> parseDefs <* parseTextWithSpaces ";")
    <|> KExprs <$> (parseExprs <* parseTextWithSpaces ";")

-- parseComs

parseComs :: Parser Coms
parseComs = parseSome (parseAnyChar allComsChars)
    where
        allComsChars = [' ' .. '"'] ++ ['$' .. ':'] ++ ['<' .. '~']

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
parseForExpr = ForExpr <$> (parseTextWithSpacesAfter "for"
                                *> parseIdAndExpr "=")
                            <*> (parseComma *> parseIdAndExpr "<" <* parseComma)
                            <*> (parseExpr <* parseTextWithSpacesAround "in")
                            <*> parseExprs
    where
        parseIdAndExpr s =
            parseAnd (parseIdentifier <* parseTextWithSpaces s) parseExpr
        parseComma = parseTextWithSpaces ","

-- parseIfExpr

parseIfExpr :: Parser IfExpr
parseIfExpr = createIfExpr
        <$> parseIfThen
        <*> (Just <$> (parseTextWithSpacesAround "else" *> parseExprs))
    <|> (`createIfExpr` Nothing) <$> parseIfThen
    where
        parseIfThen = (,) <$> (parseTextWithSpacesAfter "if" *> parseExpr)
                          <*> (parseTextWithSpacesAround "then" *> parseExprs)
        createIfExpr (i, t) e = IfExpr i t e

-- parseWhileExpr

parseWhileExpr :: Parser WhileExpr
parseWhileExpr = WhileExpr <$> (parseTextWithSpacesAfter "while" *> parseExpr)
                           <*> (parseTextWithSpacesAround "do" *> parseExprs)

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
parseCallExpr =  parseChar '(' *> parseListOfExprs <* parseCP
    <|> const [] <$ parseChar '(' <*> parseCP
    where
        parseListOfExprs = (:) <$> (parseWithSpaces parseExpr
                <* parseTextWithSpaces ",") <*> parseListOfExprs
            <|> (:[]) <$> parseWithSpaces parseExpr

-- parsePrimary

parsePrimary :: Parser Primary
parsePrimary = PBoolean <$> parseBoolean
    <|> PId <$> parseIdentifier
    <|> PLit <$> parseLiteral
    <|> PExprs <$> (parseOP *> parseExprs <* parseCP)

-- parseIdentifier

parseIdentifier :: Parser Identifier
parseIdentifier = (:) <$> parseWithSpaces (parseAnyChar allLetters)
        <*> parseSome (parseAnyChar allAlphaNums)
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

-- parseBoolean

parseBoolean :: Parser Boolean
parseBoolean = Data.True <$ parseTextWithSpaces "true"
    <|> Data.False <$ parseTextWithSpaces "false"

-- parseBinop

parseBinop :: Parser Binop
parseBinop = Mul <$ parseWithSpaces (parseChar '*')
    <|> Div <$ parseWithSpaces (parseChar '/')
    <|> Add <$ parseWithSpaces (parseChar '+')
    <|> Sub <$ parseWithSpaces (parseChar '-')
    <|> Mod <$ parseWithSpaces (parseString "%")
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
parseWithSpaces p = parseSomeOut (parseAnyChar " \t") *> p
    <|> p

parseTextWithSpaces :: String -> Parser String
parseTextWithSpaces s = parseWithSpaces (parseString s)

parseTextWithSpacesAfter :: String -> Parser String
parseTextWithSpacesAfter s = parseWithSpaces (parseString (s ++ " "))
    <|> parseWithSpaces (parseString (s ++ "\t"))

parseTextWithSpacesAround :: String -> Parser String
parseTextWithSpacesAround s =
        parseAnyChar " \t" *> parseWithSpaces parseStringWithSpaceAfter
    where
        parseStringWithSpaceAfter = parseString s <* parseAnyChar " \t"

parseOP :: Parser Char
parseOP = parseWithSpaces (parseChar '(')

parseCP :: Parser Char
parseCP = parseWithSpaces (parseChar ')')