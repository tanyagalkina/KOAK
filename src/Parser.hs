{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Parser where

-- Import

import Control.Applicative


-- Data

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

-- Instance

instance Functor Parser where
    fmap fct parser = Parser $ \s -> case runParser parser s of
        Nothing         -> Nothing
        (Just (x,y))    -> Just (fct x, y)

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    Parser f <*> p = Parser $ \s -> case f s of
        Nothing         -> Nothing
        (Just (f', s'))  -> case runParser p s' of
            Nothing         -> Nothing
            (Just (x, s'')) -> Just (f' x, s'')

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser p <|> p' = Parser $ \s -> p s <|> runParser p' s

-- General Parser

parseChar :: Char -> Parser Char
parseChar c = Parser  $ \case
    x : xs | x == c -> Just (x,xs)
    _ -> Nothing

parseAllChar :: Parser Char
parseAllChar = Parser  $ \case
                         (x:xs) -> Just (x,xs)
                         [] -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar = foldr ((<|>) . parseChar) (Parser (const Nothing))

parseString :: String -> Parser String
parseString =
    foldr (\x -> (<*>) ((:) <$> parseChar x)) (Parser (\s -> Just ("", s)))

parseAnd :: Parser a -> Parser b -> Parser (a,b)
parseAnd = parseAndWith (,)

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p p' = f <$> p <*> p'

parseMany :: Parser a -> Parser [a]
parseMany p = parseSome p <|> pure []

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

parseSomeOut :: Parser a -> Parser [a]
parseSomeOut p = (\_ _ -> []) <$> p <*> parseMany p

parseEnd :: Parser Char
parseEnd = Parser $ \case
                        [] -> Just ('e', "")
                        _ -> Nothing

-- Numeric Parser

parseTuple :: Parser a -> Parser (a,a)
parseTuple p = parseAnd (parseOP *> p ) (parseCol *> p <* parseCP)
    where
        parseCol = parseChar ','
        parseOP = parseChar '('
        parseCP = parseChar ')'

parseUIntStr :: Parser String
parseUIntStr = parseSome (parseAnyChar ['0'..'9'])

parseUInt :: Parser Int
parseUInt = read <$> parseSome (parseAnyChar ['0'..'9'])

parseInt :: Parser Int
parseInt = (\x -> -x) <$> (parseNeg *> parseUInt)
                          <|> parsePos *> parseUInt
                          <|> parseUInt
    where
        parseNeg = parseChar '-'
        parsePos = parseChar '+'

parseIntStr :: Parser String 
parseIntStr = show <$> parseInt

parseFloatStr :: Parser String
parseFloatStr = parseNum
        where
            parseDot = parseChar '.'
            myConcat = \x y z -> show x ++ [y] ++ z
            parseNum =  myConcat <$> parseInt <*> parseDot <*> parseUIntStr

parseFloatDotStr :: Parser String
parseFloatDotStr = parseNum 
        where
            parseDot = parseChar '.'
            myConcat =  \x y -> x ++ [y] ++ "0"
            parseNum =  myConcat <$> parseUIntStr <*> parseDot

parseDotFloatStr :: Parser String
parseDotFloatStr = parseNum
        where
            parseDot = parseChar '.'
            myConcat =  \y z -> "0" ++ [y] ++ z
            parseNum =  myConcat <$> parseDot <*> parseUIntStr

parseFloat :: Parser Float
parseFloat = read <$> parseFloatStr

parseDouble :: Parser Double
parseDouble = read <$> parseFloatStr

parseDotDouble :: Parser Double
parseDotDouble = read <$> (parseFloatStr <|> parseDotFloatStr <|> parseFloatDotStr)