module CreateASTSpec (spec) where

import Test.Hspec

import Parser

import Data
import ParseCode
import CreateAST
import Data (Value(VPrototype, VNothing), Type (TUndefine))

spec :: Spec
spec = do
    describe "createUnop"              testCreateUnop
    describe "createBinop"             testCreateBinop
    describe "createDoubleConst"       testCreateDoubleConst
    describe "createDecimalConst"      testCreateDecimalConst
    describe "createLiteral"           testCreateLiteral
    describe "createIdentifier"        testCreateIdentifier
    describe "createPrimary"           testCreatePrimary
    describe "createCallExpr"          testCreateCallExpr
    describe "createPostfix"           testCreatePostfix
    describe "createUnary"             testCreateUnary
    describe "createExpr"              testCreateExpr
    describe "createForExpr"           testCreateForExpr
    describe "createIfExpr"            testCreateIfExpr
    describe "createWhileExpr"         testCreateWhileExpr
    describe "createExprs"             testCreateExprs
    describe "createArgsType"          testCreateArgsType
    describe "createPrototypeArgs"     testCreatePrototypeArgs
    describe "createPrototype"         testCreatePrototype
    describe "createDefs"              testCreateDefs
    describe "createKdefs"             testCreateKdefs
    describe "createStmt"              testCreateStmt

testCreateUnop :: Spec
testCreateUnop = do
    it "createUnop Minus" $ do
        createUnop Minus `shouldBe` VUnop Minus
    it "createUnop Not" $ do
        createUnop Not `shouldBe` VUnop Not

testCreateBinop :: Spec
testCreateBinop = do
    it "createBinop Mul" $ do
        createBinop Mul `shouldBe` VBinop Mul
    it "createBinop Add" $ do
        createBinop Add `shouldBe` VBinop Add
    it "createBinop Div" $ do
        createBinop Div `shouldBe` VBinop Div
    it "createBinop Sub" $ do
        createBinop Sub `shouldBe` VBinop Sub
    it "createBinop Eq" $ do
        createBinop Eq `shouldBe` VBinop Eq
    it "createBinop Neq" $ do
        createBinop Neq `shouldBe` VBinop Neq
    it "createBinop Lt" $ do
        createBinop Lt `shouldBe` VBinop Lt
    it "createBinop Gt" $ do
        createBinop Gt `shouldBe` VBinop Gt
    it "createBinop Assign" $ do
        createBinop Assign `shouldBe` VBinop Assign

testCreateDoubleConst :: Spec
testCreateDoubleConst = do
    it "createDoubleConst 10.4" $ do
        createDoubleConst 10.4 `shouldBe` VDoubleConst 10.4
    it "createDoubleConst 0.4" $ do
        createDoubleConst 0.4 `shouldBe` VDoubleConst 0.4

testCreateDecimalConst :: Spec
testCreateDecimalConst = do
    it "createDecimalConst 10" $ do
        createDecimalConst 10 `shouldBe` VDecimalConst 10

testCreateLiteral :: Spec
testCreateLiteral = do
    it "createLiteral (LDouble 10.4)" $ do
        createLiteral (LDouble 10.4) `shouldBe` VLiteral (Node TUndefine (VDoubleConst 10.4))
    it "createLiteral (LDouble 0.4)" $ do
        createLiteral (LDouble 0.4) `shouldBe` VLiteral (Node TUndefine (VDoubleConst 0.4))
    it "createLiteral (LInt 10)" $ do
        createLiteral (LInt 10) `shouldBe` VLiteral (Node TUndefine (VDecimalConst 10))

testCreateIdentifier :: Spec
testCreateIdentifier = do
    it "createIdentifier \"Patrick\"" $ do
        createIdentifier "Patrick" `shouldBe` VIdentifier "Patrick"
    it "createIdentifier \"zaCh18\"" $ do
        createIdentifier "zaCh18" `shouldBe` VIdentifier "zaCh18"
    it "createIdentifier \"a\"" $ do
        createIdentifier "a" `shouldBe` VIdentifier "a"

testCreatePrimary :: Spec
testCreatePrimary = do
    it "createPrimary (PLit (LDouble 12.3))" $ do
        createPrimary (PLit (LDouble 12.3)) `shouldBe` VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDoubleConst 12.3))))
    it "createPrimary (PLit (LInt 12))" $ do
        createPrimary (PLit (LInt 12)) `shouldBe` VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 12))))
    it "createPrimary (PId \"guy\")" $ do
        createPrimary (PId "guy") `shouldBe` VPrimary (Node TUndefine (VIdentifier "guy"))
    it "createPrimary (PExprs (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))" $ do
        createPrimary (PExprs (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]])) `shouldBe` VPrimary (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]))

testCreateCallExpr :: Spec
testCreateCallExpr = do
    it "createCallExpr [Expr (UPostfix (Postfix (PLit (LInt 12)) Nothing)) [(Add, UPostfix (Postfix (PId \"Patrick\") Nothing))]]" $ do
        createCallExpr [Expr (UPostfix (Postfix (PLit (LInt 12)) Nothing)) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))]] `shouldBe` VCallExpr [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 12)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "Patrick")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]
    it "createCallExpr [Expr (Unop Minus (UPostfix (Postfix (PLit (LInt 12)) Nothing))) [(Add, UPostfix (Postfix (PId \"Patrick\") Nothing))],Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Div, UPostfix (Postfix (PLit (LInt 2)) Nothing))]]" $ do
        createCallExpr [Expr (Unop Minus (UPostfix (Postfix (PLit (LInt 12)) Nothing))) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))],Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Div, UPostfix (Postfix (PLit (LInt 2)) Nothing))]] `shouldBe` VCallExpr [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VUnop Minus)) (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 12)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))))) [(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "Patrick")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))]),Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Div),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 2)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]
    it "createCallExpr [Expr (UPostfix (Postfix (PId \"bonjour\") Nothing)) [(Eq, UPostfix (Postfix (PId \"aurevoir\") Nothing))]]" $ do
        createCallExpr [Expr (UPostfix (Postfix (PId "bonjour") Nothing)) [(Eq, UPostfix (Postfix (PId "aurevoir") Nothing))]] `shouldBe` VCallExpr [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "bonjour")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Eq),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "aurevoir")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]
    it "createCallExpr [Expr (UPostfix (Postfix (PId \"guy\") Nothing)) []]" $ do
        createCallExpr [Expr (UPostfix (Postfix (PId "guy") Nothing)) []] `shouldBe` VCallExpr [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "guy")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [])]
    it "createCallExpr []" $ do
        createCallExpr [] `shouldBe` VCallExpr [Node TUndefine VNothing]

testCreatePostfix :: Spec
testCreatePostfix = do
    it "createPostfix (Postfix (PLit (LDouble 12.3)) Nothing)" $ do
        createPostfix (Postfix (PLit (LDouble 12.3)) Nothing) `shouldBe` VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDoubleConst 12.3)))))) (Node TUndefine VNothing)
    it "createPostfix (Postfix (PLit (LInt 12)) Nothing)" $ do
        createPostfix (Postfix (PLit (LInt 12)) Nothing) `shouldBe` VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 12)))))) (Node TUndefine VNothing)
    it "createPostfix (Postfix (PId \"guy\") Nothing)" $ do
        createPostfix (Postfix (PId "guy") Nothing) `shouldBe` VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "guy")))) (Node TUndefine VNothing)
    it "createPostfix (Postfix (PId \"guy\") (Just []))" $ do
        createPostfix (Postfix (PId "guy") (Just [])) `shouldBe` VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "guy")))) (Node TUndefine (VCallExpr [Node TUndefine VNothing]))
    it "createPostfix (Postfix (PId \"guy\") (Just [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))" $ do
        createPostfix (Postfix (PId "guy") (Just [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]])) `shouldBe` VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "guy")))) (Node TUndefine (VCallExpr [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]))

testCreateUnary :: Spec
testCreateUnary = do
    it "createUnary (UPostfix (Postfix (PLit (LDouble 12.3)) Nothing))" $ do
        createUnary (UPostfix (Postfix (PLit (LDouble 12.3)) Nothing)) `shouldBe` VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDoubleConst 12.3)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)
    it "createUnary (UPostfix (Postfix (PLit (LInt 12)) Nothing))" $ do
        createUnary (UPostfix (Postfix (PLit (LInt 12)) Nothing)) `shouldBe` VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 12)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)
    it "createUnary (UPostfix (Postfix (PId \"guy\") Nothing))" $ do
        createUnary (UPostfix (Postfix (PId "guy") Nothing)) `shouldBe` VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "guy")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)
    it "createUnary (Unop Minus (UPostfix (Postfix (PLit (LDouble 12.3)) Nothing)))" $ do
        createUnary (Unop Minus (UPostfix (Postfix (PLit (LDouble 12.3)) Nothing))) `shouldBe` VUnary (Node TUndefine (VUnop Minus)) (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDoubleConst 12.3)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))
    it "createUnary (Unop Minus (UPostfix (Postfix (PId \"plouf\") Nothing)))" $ do
        createUnary (Unop Minus (UPostfix (Postfix (PId "plouf") Nothing))) `shouldBe` VUnary (Node TUndefine (VUnop Minus)) (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "plouf")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))
    it "createUnary (Unop Not (UPostfix (Postfix (PId \"guy\") Nothing)))" $ do
        createUnary (Unop Not (UPostfix (Postfix (PId "guy") Nothing))) `shouldBe` VUnary (Node TUndefine (VUnop Not)) (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "guy")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))

testCreateExpr :: Spec
testCreateExpr = do
    it "createExpr (Expr (UPostfix (Postfix (PLit (LInt 12)) Nothing)) [(Add, UPostfix (Postfix (PId \"Patrick\") Nothing))])" $ do
        createExpr (Expr (UPostfix (Postfix (PLit (LInt 12)) Nothing)) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))]) `shouldBe` VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 12)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "Patrick")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))]
    it "createExpr (Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing)),(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))])" $ do
        createExpr (Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing)),(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]) `shouldBe` VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))),(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))]
    it "createExpr (Expr (Unop Minus (UPostfix (Postfix (PLit (LInt 12)) Nothing))) [(Add, UPostfix (Postfix (PId \"Patrick\") Nothing))])" $ do
        createExpr (Expr (Unop Minus (UPostfix (Postfix (PLit (LInt 12)) Nothing))) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))]) `shouldBe` VExpr (Node TUndefine (VUnary (Node TUndefine (VUnop Minus)) (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 12)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))))) [(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "Patrick")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))]
    it "createExpr (Expr (UPostfix (Postfix (PId \"bonjour\") Nothing)) [(Eq, UPostfix (Postfix (PId \"aurevoir\") Nothing))])" $ do
        createExpr (Expr (UPostfix (Postfix (PId "bonjour") Nothing)) [(Eq, UPostfix (Postfix (PId "aurevoir") Nothing))]) `shouldBe` VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "bonjour")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Eq),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "aurevoir")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))]
    it "createExpr (Expr (UPostfix (Postfix (PId \"guy\") Nothing)) [])" $ do
        createExpr (Expr (UPostfix (Postfix (PId "guy") Nothing)) []) `shouldBe` VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "guy")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) []

testCreateForExpr :: Spec
testCreateForExpr = do
    it "createForExpr (ForExpr (\"a\",Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) []) (\"a\",Expr (UPostfix (Postfix (PLit (LInt 3)) Nothing)) []) (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign,UPostfix (Postfix (PId \"a\") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"b\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))" $ do
        createForExpr (ForExpr ("a",Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) []) ("a",Expr (UPostfix (Postfix (PLit (LInt 3)) Nothing)) []) (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign,UPostfix (Postfix (PId "a") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "b") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]])) `shouldBe` VForExpr (Node TUndefine (VIdentifier "a"),Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [])) (Node TUndefine (VIdentifier "a"),Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 3)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [])) (Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))),(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))),(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]))

testCreateIfExpr :: Spec
testCreateIfExpr = do
    it "createIfExpr (IfExpr (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]]) Nothing)" $ do
        createIfExpr (IfExpr (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]]) Nothing) `shouldBe` VIfExpr (Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "c")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])])) (Node TUndefine VNothing)
    it "createIfExpr (IfExpr (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]]) (Just (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId  \"a\") Nothing))]])))" $ do
        createIfExpr (IfExpr (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]]) (Just (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "a") Nothing))]]))) `shouldBe` VIfExpr (Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "c")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])])) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "c")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]))

testCreateWhileExpr :: Spec
testCreateWhileExpr = do
    it "createWhileExpr (WhileExpr (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Eq,UPostfix (Postfix (PId \"b\") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]]))" $ do
        createWhileExpr (WhileExpr (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Eq,UPostfix (Postfix (PId "b") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]])) `shouldBe` VWhileExpr (Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Eq),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "c")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]))

testCreateExprs :: Spec
testCreateExprs = do
    it "createExprs (EWhileExpr (WhileExpr (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Eq,UPostfix (Postfix (PId \"b\") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]])))" $ do
        createExprs (EWhileExpr (WhileExpr (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Eq,UPostfix (Postfix (PId "b") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]]))) `shouldBe` VExprs [Node TUndefine (VWhileExpr (Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Eq),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "c")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])])))]
    it "createExprs (EIfExpr (IfExpr (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]]) (Just (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId  \"a\") Nothing))]]))))" $ do
        createExprs (EIfExpr (IfExpr (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]]) (Just (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "a") Nothing))]])))) `shouldBe` VExprs [Node TUndefine (VIfExpr (Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "c")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])])) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "c")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])])))]
    it "createExprs (EForExpr (ForExpr (\"a\",Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) []) (\"a\",Expr (UPostfix (Postfix (PLit (LInt 3)) Nothing)) []) (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign,UPostfix (Postfix (PId \"a\") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"b\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]])))" $ do
        createExprs (EForExpr (ForExpr ("a",Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) []) ("a",Expr (UPostfix (Postfix (PLit (LInt 3)) Nothing)) []) (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign,UPostfix (Postfix (PId "a") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "b") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))) `shouldBe` VExprs [Node TUndefine (VForExpr (Node TUndefine (VIdentifier "a"),Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [])) (Node TUndefine (VIdentifier "a"),Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 3)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [])) (Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))),(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))),(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])])))]
    it "createExprs (EExpr [Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))], Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 2)) Nothing))]])" $ do
        createExprs (EExpr [Expr (UPostfix (Postfix (PId "a") Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))], Expr (UPostfix (Postfix (PId "a") Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 2)) Nothing))]]) `shouldBe` VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))]),Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 2)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]

testCreateArgsType :: Spec
testCreateArgsType = do
    it "createArgsType Int" $ do
        createArgsType Int `shouldBe` VArgsType Int
    it "createArgsType Double" $ do
        createArgsType Double `shouldBe` VArgsType Double
    it "createArgsType Void" $ do
        createArgsType Void `shouldBe` VArgsType Void

testCreatePrototypeArgs :: Spec
testCreatePrototypeArgs = do
    it "createPrototypeArgs (PrototypeArgs [(\"a\",Int)] Void)" $ do
        createPrototypeArgs (PrototypeArgs [("a",Int)] Void) `shouldBe` VPrototypeArgs [(Node TUndefine (VIdentifier "a"),Node TUndefine (VArgsType Int))] (Node TUndefine (VArgsType Void))
    it "createPrototypeArgs (PrototypeArgs [] Int)" $ do
        createPrototypeArgs (PrototypeArgs [] Int) `shouldBe` VPrototypeArgs [] (Node TUndefine (VArgsType Int))
    it "createPrototypeArgs (PrototypeArgs [(\"a\",Int),(\"b\",Double)] Double)" $ do
        createPrototypeArgs (PrototypeArgs [("a",Int),("b",Double)] Double) `shouldBe` VPrototypeArgs [(Node TUndefine (VIdentifier "a"),Node TUndefine (VArgsType Int)),(Node TUndefine (VIdentifier "b"),Node TUndefine (VArgsType Double))] (Node TUndefine (VArgsType Double))

testCreatePrototype :: Spec
testCreatePrototype = do
    it "createPrototype (Prototype \"plouf\" (PrototypeArgs [(\"a\",Int)] Void))" $ do
        createPrototype (Prototype "plouf" (PrototypeArgs [("a",Int)] Void)) `shouldBe` VPrototype (Node TUndefine (VIdentifier "plouf")) (Node TUndefine (VPrototypeArgs [(Node TUndefine (VIdentifier "a"),Node TUndefine (VArgsType Int))] (Node TUndefine (VArgsType Void))))
    it "createPrototype (Prototype \"hey\" (PrototypeArgs [] Int))" $ do
        createPrototype (Prototype "hey" (PrototypeArgs [] Int)) `shouldBe` VPrototype (Node TUndefine (VIdentifier "hey")) (Node TUndefine (VPrototypeArgs [] (Node TUndefine (VArgsType Int))))
    it "createPrototype (Prototype \"mdr\" (PrototypeArgs [(\"a\",Int),(\"b\",Double)] Double))" $ do
        createPrototype (Prototype "mdr" (PrototypeArgs [("a",Int),("b",Double)] Double)) `shouldBe` VPrototype (Node TUndefine (VIdentifier "mdr")) (Node TUndefine (VPrototypeArgs [(Node TUndefine (VIdentifier "a"),Node TUndefine (VArgsType Int)),(Node TUndefine (VIdentifier "b"),Node TUndefine (VArgsType Double))] (Node TUndefine (VArgsType Double))))

testCreateDefs :: Spec
testCreateDefs = do
    it "createDefs (Defs (Prototype \"plouf\" (PrototypeArgs [(\"a\",Int)] Void)) (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))" $ do
        createDefs (Defs (Prototype "plouf" (PrototypeArgs [("a",Int)] Void)) (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]])) `shouldBe` VDefs (Node TUndefine (VPrototype (Node TUndefine (VIdentifier "plouf")) (Node TUndefine (VPrototypeArgs [(Node TUndefine (VIdentifier "a"),Node TUndefine (VArgsType Int))] (Node TUndefine (VArgsType Void)))))) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]))
    it "createDefs (Defs (Prototype \"mdr\" (PrototypeArgs [(\"a\", Int), (\"b\", Double)] Double)) (EExpr [Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign, UPostfix (Postfix (PLit (LInt 4)) Nothing)), (Add, UPostfix (Postfix (PId \"b\") Nothing))]]))" $ do
        createDefs (Defs (Prototype "mdr" (PrototypeArgs [("a", Int), ("b", Double)] Double)) (EExpr [Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign, UPostfix (Postfix (PLit (LInt 4)) Nothing)), (Add, UPostfix (Postfix (PId "b") Nothing))]])) `shouldBe` VDefs (Node TUndefine (VPrototype (Node TUndefine (VIdentifier "mdr")) (Node TUndefine (VPrototypeArgs [(Node TUndefine (VIdentifier "a"),Node TUndefine (VArgsType Int)),(Node TUndefine (VIdentifier "b"),Node TUndefine (VArgsType Double))] (Node TUndefine (VArgsType Double)))))) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 4)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))),(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "b")))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]))

testCreateKdefs :: Spec
testCreateKdefs = do
    it "createKdefs (KDefs (Defs (Prototype \"plouf\" (PrototypeArgs [(\"a\",Int)] Void)) (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]])))" $ do
        createKdefs (KDefs (Defs (Prototype "plouf" (PrototypeArgs [("a",Int)] Void)) (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))) `shouldBe` VKdefs (Node TUndefine (VDefs (Node TUndefine (VPrototype (Node TUndefine (VIdentifier "plouf")) (Node TUndefine (VPrototypeArgs [(Node TUndefine (VIdentifier "a"),Node TUndefine (VArgsType Int))] (Node TUndefine (VArgsType Void)))))) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]))))
    it "createKdefs (KExprs (EExpr [Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign, UPostfix (Postfix (PLit (LInt 1)) Nothing)), (Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))" $ do
        createKdefs (KExprs (EExpr [Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign, UPostfix (Postfix (PLit (LInt 1)) Nothing)), (Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]])) `shouldBe` VKdefs (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "a")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Assign),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))),(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]))

testCreateStmt :: Spec
testCreateStmt = do
    it "createStmt [KDefs (Defs (Prototype \"test\" (PrototypeArgs [(\"x\",Double)]    Double))    (EExpr [Expr (UPostfix (Postfix (PId \"x\")   Nothing))  [(Add,UPostfix (Postfix (PLit (LDouble 2.0))  Nothing))]])),    KExprs (EExpr [Expr (UPostfix (Postfix (PId \"test\") (Just [Expr (UPostfix (Postfix (PLit (LDouble 5.0))  Nothing)) []])))  [(Sub, UPostfix (Postfix (PLit (LInt 2))  Nothing)),    (Mul, UPostfix (Postfix (PLit (LInt 3)) Nothing)),   (Add, UPostfix (Postfix (PLit (LInt 1))    Nothing))]])]" $ do
        createStmt [KDefs (Defs (Prototype "test" (PrototypeArgs [("x",Double)]    Double))    (EExpr [Expr (UPostfix (Postfix (PId "x")   Nothing))  [(Add,UPostfix (Postfix (PLit (LDouble 2.0))  Nothing))]])),    KExprs (EExpr [Expr (UPostfix (Postfix (PId "test") (Just [Expr (UPostfix (Postfix (PLit (LDouble 5.0))  Nothing)) []])))  [(Sub, UPostfix (Postfix (PLit (LInt 2))  Nothing)),    (Mul, UPostfix (Postfix (PLit (LInt 3)) Nothing)),   (Add, UPostfix (Postfix (PLit (LInt 1))    Nothing))]])]
            `shouldBe` VStmt [Node TUndefine (VKdefs (Node TUndefine (VDefs (Node TUndefine (VPrototype (Node TUndefine (VIdentifier "test")) (Node TUndefine (VPrototypeArgs [(Node TUndefine (VIdentifier "x"),Node TUndefine (VArgsType Double))] (Node TUndefine (VArgsType Double)))))) (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "x")))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDoubleConst 2.0)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])]))))),Node TUndefine (VKdefs (Node TUndefine (VExprs [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VIdentifier "test")))) (Node TUndefine (VCallExpr [Node TUndefine (VExpr (Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDoubleConst 5.0)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))) [])])))) (Node TUndefine VNothing))) [(Node TUndefine (VBinop Sub),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 2)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))),(Node TUndefine (VBinop Mul),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 3)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing))),(Node TUndefine (VBinop Add),Node TUndefine (VUnary (Node TUndefine (VPostfix (Node TUndefine (VPrimary (Node TUndefine (VLiteral (Node TUndefine (VDecimalConst 1)))))) (Node TUndefine VNothing))) (Node TUndefine VNothing)))])])))]