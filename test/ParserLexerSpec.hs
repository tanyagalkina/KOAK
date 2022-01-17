module ParserLexerSpec (spec) where

import Test.Hspec

import Parser
import Data

import ParserLexer

spec :: Spec
spec = do
    describe "parseUnop"              testParseUnop
    describe "parseBinop"             testParseBinop
    describe "parseDoubleConst"       testParseDoubleConst
    describe "parseDecimalConst"      testParseDecimalConst
    describe "parseLiteral"           testParseLiteral
    describe "parseIdentifier"        testParseIdentifier
    describe "parsePrimary"           testParsePrimary
    describe "parseCallExpr"          testParseCallExpr
    describe "parsePostfix"           testParsePostfix
    describe "parseUnary"             testParseUnary
    describe "parseExpr"              testParseExpr

testParseUnop :: Spec
testParseUnop = do
    it "runParser parseUnop \"-\"" $ do
        runParser parseUnop "-" `shouldBe` Just (Minus, "")
    it "runParser parseUnop \"!\"" $ do
        runParser parseUnop "!" `shouldBe` Just (Not, "")
    it "runParser parseUnop \"-  \"" $ do
        runParser parseUnop "-  " `shouldBe` Just (Minus, "")
    it "runParser parseUnop \"!  \"" $ do
        runParser parseUnop "!  " `shouldBe` Just (Not, "")
    it "runParser parseUnop \"a\"" $ do
        runParser parseUnop "a" `shouldBe` Nothing

testParseBinop :: Spec
testParseBinop = do
    it "runParser parseBinop \"* \"" $ do
        runParser parseBinop "* " `shouldBe` Just (Mul, "")
    it "runParser parseBinop \"+ \"" $ do
        runParser parseBinop "+ " `shouldBe` Just (Add, "")
    it "runParser parseBinop \"/ \"" $ do
        runParser parseBinop "/ " `shouldBe` Just (Div, "")
    it "runParser parseBinop \"- \"" $ do
        runParser parseBinop "- " `shouldBe` Just (Sub, "")
    it "runParser parseBinop \"== \"" $ do
        runParser parseBinop "== " `shouldBe` Just (Eq, "")
    it "runParser parseBinop \"!= \"" $ do
        runParser parseBinop "!= " `shouldBe` Just (Neq, "")
    it "runParser parseBinop \"< \"" $ do
        runParser parseBinop "< " `shouldBe` Just (Lt, "")
    it "runParser parseBinop \"> \"" $ do
        runParser parseBinop "> " `shouldBe` Just (Gt, "")
    it "runParser parseBinop \"= \"" $ do
        runParser parseBinop "= " `shouldBe` Just (Assign, "")
    it "runParser parseBinop \"a\"" $ do
        runParser parseBinop "a" `shouldBe` Nothing

testParseDoubleConst :: Spec
testParseDoubleConst = do
    it "runParser parseDoubleConst \"10.4  \"" $ do
        runParser parseDoubleConst "10.4  " `shouldBe` Just (10.4, "")
    it "runParser parseDoubleConst \".4  \"" $ do
        runParser parseDoubleConst ".4  " `shouldBe` Just (0.4, "")
    it "runParser parseDoubleConst \"10  \"" $ do
        runParser parseDoubleConst "10  " `shouldBe` Nothing
    it "runParser parseDoubleConst \"a\"" $ do
        runParser parseDoubleConst "a" `shouldBe` Nothing

testParseDecimalConst :: Spec
testParseDecimalConst = do
    it "runParser parseDecimalConst \"10.4  \"" $ do
        runParser parseDecimalConst "10.4  " `shouldBe` Just (10, ".4  ")
    it "runParser parseDecimalConst \".4  \"" $ do
        runParser parseDecimalConst ".4  " `shouldBe` Nothing
    it "runParser parseDecimalConst \"10  \"" $ do
        runParser parseDecimalConst "10  " `shouldBe` Just (10, "")
    it "runParser parseDecimalConst \"a\"" $ do
        runParser parseDecimalConst "a" `shouldBe` Nothing

testParseLiteral :: Spec
testParseLiteral = do
    it "runParser parseLiteral \"10.4  \"" $ do
        runParser parseLiteral "10.4  " `shouldBe` Just (LDouble 10.4, "")
    it "runParser parseLiteral \".4  \"" $ do
        runParser parseLiteral ".4  " `shouldBe` Just (LDouble 0.4, "")
    it "runParser parseLiteral \"10  \"" $ do
        runParser parseLiteral "10  " `shouldBe` Just (LInt 10, "")
    it "runParser parseLiteral \"a\"" $ do
        runParser parseLiteral "a" `shouldBe` Nothing

testParseIdentifier :: Spec
testParseIdentifier = do
    it "runParser parseIdentifier \"Patrick  \"" $ do
        runParser parseIdentifier "Patrick  " `shouldBe` Just ("Patrick", "")
    it "runParser parseIdentifier \"zaCh18\"" $ do
        runParser parseIdentifier "zaCh18" `shouldBe` Just ("zaCh18", "")
    it "runParser parseIdentifier \"a\"" $ do
        runParser parseIdentifier "a" `shouldBe` Just ("a", "")
    it "runParser parseIdentifier \"12sim\"" $ do
        runParser parseIdentifier "12sim" `shouldBe` Nothing
    it "runParser parseIdentifier \"\"" $ do
        runParser parseIdentifier "" `shouldBe` Nothing

testParsePrimary :: Spec
testParsePrimary = do
    it "runParser parsePrimary \"12.3  \"" $ do
        runParser parsePrimary "12.3  " `shouldBe` Just (Lit (LDouble 12.3), "")
    it "runParser parsePrimary \"12\"" $ do
        runParser parsePrimary "12" `shouldBe` Just (Lit (LInt 12), "")
    it "runParser parsePrimary \"guy  \"" $ do
        runParser parsePrimary "guy  " `shouldBe` Just (Id "guy", "")
--    it "runParser parsePrimary \"(exprs)\"" $ do
--        runParser parsePrimary "(exprs)" `shouldBe` Nothing
    it "runParser parsePrimary \" \"" $ do
        runParser parsePrimary " " `shouldBe` Nothing

testParseCallExpr :: Spec
testParseCallExpr = do
    it "runParser parseCallExpr \"(12  +  Patrick  )\"" $ do
        runParser parseCallExpr "(12  +  Patrick  )"
            `shouldBe` Just ([Expr (UPostfix (Postfix (Lit (LInt 12)) Nothing)) [(Add, EUnary (UPostfix (Postfix (Id "Patrick") Nothing)))]], "")
    it "runParser parseCallExpr \"(- 12  +  Patrick  ,   1 / 2)\"" $ do
        runParser parseCallExpr "(- 12  +  Patrick  ,   1 / 2)"
        `shouldBe` Just ([Expr (Unop Minus (UPostfix (Postfix (Lit (LInt 12)) Nothing))) [(Add,EUnary (UPostfix (Postfix (Id "Patrick") Nothing)))]
                         ,Expr (UPostfix (Postfix (Lit (LInt 1)) Nothing)) [(Div,EUnary (UPostfix (Postfix (Lit (LInt 2)) Nothing)))]], "")
    it "runParser parseCallExpr \"(bonjour == aurevoir  )  \"" $ do
        runParser parseCallExpr "(bonjour == aurevoir  )  "
        `shouldBe` Just ([Expr (UPostfix (Postfix (Id "bonjour") Nothing)) [(Eq,EUnary (UPostfix (Postfix (Id "aurevoir") Nothing)))]], "")
    it "runParser parseCallExpr \"(guy  )\"" $ do
        runParser parseCallExpr "(guy  )" `shouldBe` Just ([Expr (UPostfix (Postfix (Id "guy") Nothing)) []], "")
    it "runParser parseCallExpr \"1 = 2\"" $ do
        runParser parseCallExpr "1 = 2" `shouldBe` Nothing
    it "runParser parseCallExpr \"()\"" $ do
        runParser parseCallExpr "()" `shouldBe` Just ([], "")
    it "runParser parseCallExpr \" \"" $ do
        runParser parseCallExpr " " `shouldBe` Nothing

testParsePostfix :: Spec
testParsePostfix = do
    it "runParser parsePostfix \"12.3  \"" $ do
        runParser parsePostfix "12.3  " `shouldBe` Just (Postfix (Lit (LDouble 12.3)) Nothing, "")
    it "runParser parsePostfix \"12\"" $ do
        runParser parsePostfix "12" `shouldBe` Just (Postfix (Lit (LInt 12)) Nothing, "")
    it "runParser parsePostfix \"guy  \"" $ do
        runParser parsePostfix "guy  " `shouldBe` Just (Postfix (Id "guy") Nothing, "")
    it "runParser parsePostfix \"guy  ()\"" $ do
        runParser parsePostfix "guy  ()" `shouldBe` Just (Postfix (Id "guy") (Just []), "")
    it "runParser parsePostfix \"guy  (1 + 1)\"" $ do
        runParser parsePostfix "guy  (1 + 1)"
        `shouldBe` Just (Postfix (Id "guy") (Just [Expr (UPostfix (Postfix (Lit (LInt 1)) Nothing)) [(Add,EUnary (UPostfix (Postfix (Lit (LInt 1)) Nothing)))]]), "")
    it "runParser parsePostfix \"()\"" $ do
        runParser parsePostfix "()" `shouldBe` Nothing
    it "runParser parsePostfix \" \"" $ do
        runParser parsePostfix " " `shouldBe` Nothing

testParseUnary :: Spec
testParseUnary = do
    it "runParser parseUnary \"12.3  \"" $ do
        runParser parseUnary "12.3  " `shouldBe` Just (UPostfix (Postfix (Lit (LDouble 12.3)) Nothing), "")
    it "runParser parseUnary \"12\"" $ do
        runParser parseUnary "12" `shouldBe` Just (UPostfix (Postfix (Lit (LInt 12)) Nothing), "")
    it "runParser parseUnary \"guy  \"" $ do
        runParser parseUnary "guy  " `shouldBe` Just (UPostfix (Postfix (Id "guy") Nothing), "")
    it "runParser parseUnary \"-12.3  \"" $ do
        runParser parseUnary "-12.3  " `shouldBe` Just (Unop Minus (UPostfix (Postfix (Lit (LDouble 12.3)) Nothing)), "")
    it "runParser parseUnary \"- plouf\"" $ do
        runParser parseUnary "- plouf" `shouldBe` Just (Unop Minus (UPostfix (Postfix (Id "plouf") Nothing)), "")
    it "runParser parseUnary \"!   guy  \"" $ do
        runParser parseUnary "!   guy  " `shouldBe` Just (Unop Not (UPostfix (Postfix (Id "guy") Nothing)), "")
    it "runParser parseUnary \" \"" $ do
        runParser parseUnary " " `shouldBe` Nothing

testParseExpr :: Spec
testParseExpr = do
    it "runParser parseExpr \"12  +  Patrick  \"" $ do
        runParser parseExpr "12  +  Patrick  "
            `shouldBe` Just (Expr (UPostfix (Postfix (Lit (LInt 12)) Nothing)) [(Add, EUnary (UPostfix (Postfix (Id "Patrick") Nothing)))], "")
    it "runParser parseExpr \"1 + 1 + 1 \"" $ do
        runParser parseExpr "1 + 1 + 1 "
            `shouldBe` Just (Expr (UPostfix (Postfix (Lit (LInt 1)) Nothing)) [(Add,EUnary (UPostfix (Postfix (Lit (LInt 1)) Nothing))),(Add,EUnary (UPostfix (Postfix (Lit (LInt 1)) Nothing)))], "")
    it "runParser parseExpr \"- 12  +  Patrick  \"" $ do
        runParser parseExpr "- 12  +  Patrick  "
        `shouldBe` Just (Expr (Unop Minus (UPostfix (Postfix (Lit (LInt 12)) Nothing))) [(Add,EUnary (UPostfix (Postfix (Id "Patrick") Nothing)))], "")
    it "runParser parseExpr \"bonjour == aurevoir    \"" $ do
        runParser parseExpr "bonjour == aurevoir    "
        `shouldBe` Just (Expr (UPostfix (Postfix (Id "bonjour") Nothing)) [(Eq,EUnary (UPostfix (Postfix (Id "aurevoir") Nothing)))], "")
    it "runParser parseExpr \"guy  \"" $ do
        runParser parseExpr "guy  " `shouldBe` Just (Expr (UPostfix (Postfix (Id "guy") Nothing)) [], "")
    it "runParser parseExpr \"+ +\"" $ do
        runParser parseExpr "+ +" `shouldBe` Nothing
    it "runParser parseExpr \" \"" $ do
        runParser parseExpr " " `shouldBe` Nothing