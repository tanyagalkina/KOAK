module CreateASTSpec (spec) where

import Test.Hspec

import qualified Data.Map as Map

import Parser

import Data
import ParseCode
import CreateAST

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
        createUnop Minus
            `shouldBe` VUnop Minus
    it "createUnop Not" $ do
        createUnop Not
            `shouldBe` VUnop Not

testCreateBinop :: Spec
testCreateBinop = do
    it "createBinop Mul" $ do
        createBinop Mul
            `shouldBe` VBinop Mul
    it "createBinop Add" $ do
        createBinop Add
            `shouldBe` VBinop Add
    it "createBinop Div" $ do
        createBinop Div
            `shouldBe` VBinop Div
    it "createBinop Sub" $ do
        createBinop Sub
            `shouldBe` VBinop Sub
    it "createBinop Eq" $ do
        createBinop Eq
            `shouldBe` VBinop Eq
    it "createBinop Neq" $ do
        createBinop Neq
            `shouldBe` VBinop Neq
    it "createBinop Lt" $ do
        createBinop Lt
            `shouldBe` VBinop Lt
    it "createBinop Gt" $ do
        createBinop Gt
            `shouldBe` VBinop Gt
    it "createBinop Assign" $ do
        createBinop Assign
            `shouldBe` VBinop Assign

testCreateDoubleConst :: Spec
testCreateDoubleConst = do
    it "createDoubleConst 10.4" $ do
        createDoubleConst 10.4
            `shouldBe` VDoubleConst 10.4
    it "createDoubleConst 0.4" $ do
        createDoubleConst 0.4
            `shouldBe` VDoubleConst 0.4

testCreateDecimalConst :: Spec
testCreateDecimalConst = do
    it "createDecimalConst 10" $ do
        createDecimalConst 10
            `shouldBe` VDecimalConst 10

testCreateLiteral :: Spec
testCreateLiteral = do
    it "createLiteral (LDouble 10.4)" $ do
        createLiteral (LDouble 10.4)
            `shouldBe` VLiteral (Node TDouble (VDoubleConst 10.4))
    it "createLiteral (LDouble 0.4)" $ do
        createLiteral (LDouble 0.4)
            `shouldBe` VLiteral (Node TDouble (VDoubleConst 0.4))
    it "createLiteral (LInt 10)" $ do
        createLiteral (LInt 10)
            `shouldBe` VLiteral (Node TInteger (VDecimalConst 10))

testCreateIdentifier :: Spec
testCreateIdentifier = do
    it "createId Map.empty \"Patrick\"" $ do
        createId Map.empty "Patrick"
            `shouldBe` VIdentifier ("Patrick", Prelude.True)
    it "createId Map.empty \"zaCh18\"" $ do
        createId Map.empty "zaCh18"
            `shouldBe` VIdentifier ("zaCh18", Prelude.True)
    it "createId Map.empty \"a\"" $ do
        createId Map.empty "a"
            `shouldBe` VIdentifier ("a", Prelude.True)

testCreatePrimary :: Spec
testCreatePrimary = do
    it "createPrimary Map.empty (PLit (LDouble 12.3))" $ do
        createPrimary Map.empty (PLit (LDouble 12.3))
            `shouldBe` (VPrimary (Node TDouble (VLiteral (Node TDouble (VDoubleConst 12.3)))), Map.empty)
    it "createPrimary Map.empty (PLit (LInt 12))" $ do
        createPrimary Map.empty (PLit (LInt 12))
            `shouldBe` (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 12)))), Map.empty)
    it "createPrimary Map.empty (PId \"guy\")" $ do
        createPrimary Map.empty (PId "guy")
            `shouldBe` (VPrimary (Node TNone (VIdentifier ("guy", Prelude.True))), Map.empty)
    it "createPrimary Map.empty (PExprs (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))" $ do
        createPrimary Map.empty (PExprs (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))
            `shouldBe` (VPrimary (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])])), Map.empty)

testCreateCallExpr :: Spec
testCreateCallExpr = do
    it "createCallExpr (Map.fromList [(\"Patrick\", TInteger)]) [Expr (UPostfix (Postfix (PLit (LInt 12)) Nothing)) [(Add, UPostfix (Postfix (PId \"Patrick\") Nothing))]]" $ do
        createCallExpr (Map.fromList [("Patrick", TInteger)]) [Expr (UPostfix (Postfix (PLit (LInt 12)) Nothing)) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))]]
            `shouldBe` (VCallExpr [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 12)))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("Patrick", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])], Map.fromList [("Patrick",TInteger)])
    it "createCallExpr (Map.fromList [(\"Patrick\", TInteger)]) [Expr (Unop Minus (UPostfix (Postfix (PLit (LInt 12)) Nothing))) [(Add, UPostfix (Postfix (PId \"Patrick\") Nothing))],Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Div, UPostfix (Postfix (PLit (LInt 2)) Nothing))]]" $ do
        createCallExpr (Map.fromList [("Patrick", TInteger)]) [Expr (Unop Minus (UPostfix (Postfix (PLit (LInt 12)) Nothing))) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))],Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Div, UPostfix (Postfix (PLit (LInt 2)) Nothing))]]
            `shouldBe` (VCallExpr [Node TInteger (VExpr (Node TInteger (VUnary (Node TNone (VUnop Minus)) (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 12)))))) (Node TNone VNothing))) (Node TNone VNothing))))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("Patrick", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))]),Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Div),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 2)))))) (Node TNone VNothing))) (Node TNone VNothing)))])], Map.fromList [("Patrick",TInteger)])
    it "createCallExpr (Map.fromList [(\"bonjour\", TInteger), (\"aurevoir\", TInteger)]) [Expr (UPostfix (Postfix (PId \"bonjour\") Nothing)) [(Eq, UPostfix (Postfix (PId \"aurevoir\") Nothing))]]" $ do
        createCallExpr (Map.fromList [("bonjour", TInteger), ("aurevoir", TInteger)]) [Expr (UPostfix (Postfix (PId "bonjour") Nothing)) [(Eq, UPostfix (Postfix (PId "aurevoir") Nothing))]]
            `shouldBe` (VCallExpr [Node TBool (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("bonjour", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Eq),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("aurevoir", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])], Map.fromList [("aurevoir",TInteger),("bonjour",TInteger)])
    it "createCallExpr (Map.fromList [(\"guy\", TDouble)]) [Expr (UPostfix (Postfix (PId \"guy\") Nothing)) []]" $ do
        createCallExpr (Map.fromList [("guy", TDouble)]) [Expr (UPostfix (Postfix (PId "guy") Nothing)) []]
            `shouldBe` (VCallExpr [Node TDouble (VExpr (Node TDouble (VUnary (Node TDouble (VPostfix (Node TDouble (VPrimary (Node TDouble (VIdentifier ("guy", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [])], Map.fromList [("guy",TDouble)])
    it "createCallExpr Map.empty []" $ do
        createCallExpr Map.empty []
            `shouldBe` (VCallExpr [], Map.empty)

testCreatePostfix :: Spec
testCreatePostfix = do
    it "createPostfix Map.empty (Postfix (PLit (LDouble 12.3)) Nothing)" $ do
        createPostfix Map.empty (Postfix (PLit (LDouble 12.3)) Nothing)
            `shouldBe` (VPostfix (Node TDouble (VPrimary (Node TDouble (VLiteral (Node TDouble (VDoubleConst 12.3)))))) (Node TNone VNothing), Map.empty)
    it "createPostfix Map.empty (Postfix (PLit (LInt 12)) Nothing)" $ do
        createPostfix Map.empty (Postfix (PLit (LInt 12)) Nothing)
            `shouldBe` (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 12)))))) (Node TNone VNothing), Map.empty)
    it "createPostfix Map.empty (Postfix (PId \"guy\") Nothing)" $ do
        createPostfix Map.empty (Postfix (PId "guy") Nothing)
            `shouldBe` (VPostfix (Node TNone (VPrimary (Node TNone (VIdentifier ("guy", Prelude.True))))) (Node TNone VNothing), Map.empty)
    it "createPostfix (Map.fromList [(\"guy\", TFunc [TVoid])]) (Postfix (PId \"guy\") (Just []))" $ do
        createPostfix (Map.fromList [("guy", TFunc [TVoid])]) (Postfix (PId "guy") (Just []))
            `shouldBe` (VPostfix (Node (TFunc [TVoid]) (VPrimary (Node (TFunc [TVoid]) (VIdentifier ("guy", Prelude.False))))) (Node (TFunc [TVoid]) (VCallExpr [])), Map.fromList [("guy",TFunc [TVoid])])
    it "createPostfix (Map.fromList [(\"guy\", TFunc [TInteger, TDouble])]) (Postfix (PId \"guy\") (Just [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))" $ do
        createPostfix (Map.fromList [("guy", TFunc [TInteger, TDouble])]) (Postfix (PId "guy") (Just [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))
            `shouldBe` (VPostfix (Node (TFunc [TInteger,TDouble]) (VPrimary (Node (TFunc [TInteger,TDouble]) (VIdentifier ("guy", Prelude.False))))) (Node (TFunc [TInteger,TDouble]) (VCallExpr [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])])), Map.fromList [("guy",TFunc [TInteger,TDouble])])

testCreateUnary :: Spec
testCreateUnary = do
    it "createUnary Map.empty (UPostfix (Postfix (PLit (LDouble 12.3)) Nothing))" $ do
        createUnary Map.empty (UPostfix (Postfix (PLit (LDouble 12.3)) Nothing))
            `shouldBe` (VUnary (Node TDouble (VPostfix (Node TDouble (VPrimary (Node TDouble (VLiteral (Node TDouble (VDoubleConst 12.3)))))) (Node TNone VNothing))) (Node TNone VNothing), Map.empty)
    it "createUnary Map.empty (UPostfix (Postfix (PLit (LInt 12)) Nothing))" $ do
        createUnary Map.empty (UPostfix (Postfix (PLit (LInt 12)) Nothing))
            `shouldBe` (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 12)))))) (Node TNone VNothing))) (Node TNone VNothing), Map.empty)
    it "createUnary Map.empty (UPostfix (Postfix (PId \"guy\") Nothing))" $ do
        createUnary Map.empty (UPostfix (Postfix (PId "guy") Nothing))
            `shouldBe` (VUnary (Node TNone (VPostfix (Node TNone (VPrimary (Node TNone (VIdentifier ("guy", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing), Map.empty)
    it "createUnary Map.empty (Unop Minus (UPostfix (Postfix (PLit (LDouble 12.3)) Nothing)))" $ do
        createUnary Map.empty (Unop Minus (UPostfix (Postfix (PLit (LDouble 12.3)) Nothing)))
            `shouldBe` (VUnary (Node TNone (VUnop Minus)) (Node TDouble (VUnary (Node TDouble (VPostfix (Node TDouble (VPrimary (Node TDouble (VLiteral (Node TDouble (VDoubleConst 12.3)))))) (Node TNone VNothing))) (Node TNone VNothing))), Map.empty)
    it "createUnary Map.empty (Unop Minus (UPostfix (Postfix (PId \"plouf\") Nothing)))" $ do
        createUnary Map.empty (Unop Minus (UPostfix (Postfix (PId "plouf") Nothing)))
            `shouldBe` (VUnary (Node TNone (VUnop Minus)) (Node TNone (VUnary (Node TNone (VPostfix (Node TNone (VPrimary (Node TNone (VIdentifier ("plouf", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))), Map.empty)
    it "createUnary Map.empty (Unop Not (UPostfix (Postfix (PId \"guy\") Nothing)))" $ do
        createUnary Map.empty (Unop Not (UPostfix (Postfix (PId "guy") Nothing)))
            `shouldBe` (VUnary (Node TNone (VUnop Not)) (Node TNone (VUnary (Node TNone (VPostfix (Node TNone (VPrimary (Node TNone (VIdentifier ("guy", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))), Map.empty)

testCreateExpr :: Spec
testCreateExpr = do
    it "createExpr Map.empty (Expr (UPostfix (Postfix (PLit (LInt 12)) Nothing)) [(Add, UPostfix (Postfix (PId \"Patrick\") Nothing))])" $ do
        createExpr Map.empty (Expr (UPostfix (Postfix (PLit (LInt 12)) Nothing)) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))])
            `shouldBe` (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 12)))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TNone (VUnary (Node TNone (VPostfix (Node TNone (VPrimary (Node TNone (VIdentifier ("Patrick", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing)))], Map.empty, 0)
    it "createExpr Map.empty (Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing)),(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))])" $ do
        createExpr Map.empty (Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing)),(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))])
            `shouldBe` (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))),(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))], Map.empty, 0)
    it "createExpr Map.empty (Expr (Unop Minus (UPostfix (Postfix (PLit (LInt 12)) Nothing))) [(Add, UPostfix (Postfix (PId \"Patrick\") Nothing))])" $ do
        createExpr Map.empty (Expr (Unop Minus (UPostfix (Postfix (PLit (LInt 12)) Nothing))) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))])
            `shouldBe` (VExpr (Node TInteger (VUnary (Node TNone (VUnop Minus)) (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 12)))))) (Node TNone VNothing))) (Node TNone VNothing))))) [(Node TNone (VBinop Add),Node TNone (VUnary (Node TNone (VPostfix (Node TNone (VPrimary (Node TNone (VIdentifier ("Patrick", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing)))], Map.empty, 0)
    it "createExpr Map.empty (Expr (UPostfix (Postfix (PId \"bonjour\") Nothing)) [(Eq, UPostfix (Postfix (PId \"aurevoir\") Nothing))])" $ do
        createExpr Map.empty (Expr (UPostfix (Postfix (PId "bonjour") Nothing)) [(Eq, UPostfix (Postfix (PId "aurevoir") Nothing))])
            `shouldBe` (VExpr (Node TNone (VUnary (Node TNone (VPostfix (Node TNone (VPrimary (Node TNone (VIdentifier ("bonjour", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Eq),Node TNone (VUnary (Node TNone (VPostfix (Node TNone (VPrimary (Node TNone (VIdentifier ("aurevoir", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing)))], Map.empty, 1)
    it "createExpr Map.empty (Expr (UPostfix (Postfix (PId \"guy\") Nothing)) [])" $ do
        createExpr Map.empty (Expr (UPostfix (Postfix (PId "guy") Nothing)) [])
            `shouldBe` (VExpr (Node TNone (VUnary (Node TNone (VPostfix (Node TNone (VPrimary (Node TNone (VIdentifier ("guy", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))) [], Map.empty, 0)

testCreateForExpr :: Spec
testCreateForExpr = do
    it "createForExpr (Map.fromList [(\"b\", TInteger)]) (ForExpr (\"a\",Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) []) (\"a\",Expr (UPostfix (Postfix (PLit (LInt 3)) Nothing)) []) (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign,UPostfix (Postfix (PId \"a\") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"b\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))" $ do
        createForExpr (Map.fromList [("b", TInteger)]) (ForExpr ("a",Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) []) ("a",Expr (UPostfix (Postfix (PLit (LInt 3)) Nothing)) []) (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign,UPostfix (Postfix (PId "a") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "b") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))
            `shouldBe` (VForExpr (Node TInteger (VIdentifier ("a", Prelude.True)),Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))) [])) (Node TInteger (VIdentifier ("a", Prelude.True)),Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 3)))))) (Node TNone VNothing))) (Node TNone VNothing))) [])) (Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))),(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])) (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))),(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])])), Map.fromList [("a",TInteger),("b",TInteger)])

testCreateIfExpr :: Spec
testCreateIfExpr = do
    it "createIfExpr (Map.fromList [(\"b\", TInteger)]) (IfExpr (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]]) Nothing)" $ do
        createIfExpr (Map.fromList [("b", TInteger)]) (IfExpr (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]]) Nothing)
            `shouldBe` (VIfExpr (Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])) (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("c", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])])) (Node TNone VNothing), Map.fromList [("a",TInteger),("b",TInteger),("c",TInteger)])
    it "createIfExpr (Map.fromList [(\"b\", TInteger)]) (IfExpr (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]]) (Just (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId  \"a\") Nothing))]])))" $ do
        createIfExpr (Map.fromList [("b", TInteger)]) (IfExpr (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]]) (Just (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "a") Nothing))]])))
            `shouldBe` (VIfExpr (Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])) (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("c", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])])) (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("c", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])])), Map.fromList [("a",TInteger),("b",TInteger),("c",TInteger)])

testCreateWhileExpr :: Spec
testCreateWhileExpr = do
    it "createWhileExpr (Map.fromList [(\"a\", TInteger), (\"b\", TInteger)]) (WhileExpr (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Eq,UPostfix (Postfix (PId \"b\") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]]))" $ do
        createWhileExpr (Map.fromList [("a", TInteger), ("b", TInteger)]) (WhileExpr (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Eq,UPostfix (Postfix (PId "b") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]]))
            `shouldBe` (VWhileExpr (Node TBool (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Eq),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])) (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("c", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])])), Map.fromList [("a",TInteger),("b",TInteger),("c",TInteger)])

testCreateExprs :: Spec
testCreateExprs = do
    it "createExprs (Map.fromList [(\"a\", TInteger), (\"b\", TInteger)]) (EWhileExpr (WhileExpr (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Eq,UPostfix (Postfix (PId \"b\") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]])))" $ do
        createExprs (Map.fromList [("a", TInteger), ("b", TInteger)]) (EWhileExpr (WhileExpr (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Eq,UPostfix (Postfix (PId "b") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]])))
            `shouldBe` (VExprs [Node TInteger (VWhileExpr (Node TBool (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Eq),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])) (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("c", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])])))], Map.fromList [("a",TInteger),("b",TInteger),("c",TInteger)])
    it "createExprs (Map.fromList [(\"b\", TInteger)]) (EIfExpr (IfExpr (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing))]]) (Just (EExpr [Expr (UPostfix (Postfix (PId \"c\") Nothing)) [(Assign,UPostfix (Postfix (PId  \"a\") Nothing))]]))))" $ do
        createExprs (Map.fromList [("b", TInteger)]) (EIfExpr (IfExpr (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]]) (Just (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "a") Nothing))]]))))
            `shouldBe` (VExprs [Node TInteger (VIfExpr (Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])) (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("c", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])])) (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("c", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])])))], Map.fromList [("a",TInteger),("b",TInteger),("c",TInteger)])
    it "createExprs (Map.fromList [(\"b\", TInteger)]) (EForExpr (ForExpr (\"a\",Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) []) (\"a\",Expr (UPostfix (Postfix (PLit (LInt 3)) Nothing)) []) (Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign,UPostfix (Postfix (PId \"a\") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId \"b\") Nothing)) [(Assign,UPostfix (Postfix (PId \"b\") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]])))" $ do
        createExprs (Map.fromList [("b", TInteger)]) (EForExpr (ForExpr ("a",Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) []) ("a",Expr (UPostfix (Postfix (PLit (LInt 3)) Nothing)) []) (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign,UPostfix (Postfix (PId "a") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "b") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]])))
            `shouldBe` (VExprs [Node TInteger (VForExpr (Node TInteger (VIdentifier ("a", Prelude.True)),Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))) [])) (Node TInteger (VIdentifier ("a", Prelude.True)),Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 3)))))) (Node TNone VNothing))) (Node TNone VNothing))) [])) (Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))),(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])) (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))),(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])])))], Map.fromList [("a",TInteger),("b",TInteger)])
    it "createExprs (Map.fromList [(\"a\", TInteger)]) (EExpr [Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))], Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 2)) Nothing))]])" $ do
        createExprs (Map.fromList [("a", TInteger)]) (EExpr [Expr (UPostfix (Postfix (PId "a") Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))], Expr (UPostfix (Postfix (PId "a") Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 2)) Nothing))]])
            `shouldBe` (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))]),Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 2)))))) (Node TNone VNothing))) (Node TNone VNothing)))])], Map.fromList [("a",TInteger)])

testCreateArgsType :: Spec
testCreateArgsType = do
    it "createArgsType Int" $ do
        createArgsType Int
            `shouldBe` VArgsType Int
    it "createArgsType Double" $ do
        createArgsType Double
            `shouldBe` VArgsType Double
    it "createArgsType Void" $ do
        createArgsType Void
            `shouldBe` VArgsType Void

testCreatePrototypeArgs :: Spec
testCreatePrototypeArgs = do
    it "createPrototypeArgs Map.empty (PrototypeArgs [(\"a\",Int)] Void)" $ do
        createPrototypeArgs Map.empty (PrototypeArgs [("a",Int)] Void)
            `shouldBe` VPrototypeArgs [(Node TInteger (VIdentifier ("a", Prelude.True)),Node TInteger (VArgsType Int))] (Node TVoid (VArgsType Void))
    it "createPrototypeArgs Map.empty (PrototypeArgs [] Int)" $ do
        createPrototypeArgs Map.empty (PrototypeArgs [] Int)
            `shouldBe` VPrototypeArgs [] (Node TInteger (VArgsType Int))
    it "createPrototypeArgs Map.empty (PrototypeArgs [(\"a\",Int),(\"b\",Double)] Double)" $ do
        createPrototypeArgs Map.empty (PrototypeArgs [("a",Int),("b",Double)] Double)
            `shouldBe` VPrototypeArgs [(Node TInteger (VIdentifier ("a", Prelude.True)),Node TInteger (VArgsType Int)),(Node TDouble (VIdentifier ("b", Prelude.True)),Node TDouble (VArgsType Double))] (Node TDouble (VArgsType Double))

testCreatePrototype :: Spec
testCreatePrototype = do
    it "createPrototype Map.empty (Prototype \"plouf\" (PrototypeArgs [(\"a\",Int)] Void))" $ do
        createPrototype Map.empty (Prototype "plouf" (PrototypeArgs [("a",Int)] Void))
            `shouldBe` (VPrototype (Node (TFunc [TInteger,TVoid]) (VIdentifier ("plouf", Prelude.True))) (Node (TFunc [TInteger,TVoid]) (VPrototypeArgs [(Node TInteger (VIdentifier ("a", Prelude.True)),Node TInteger (VArgsType Int))] (Node TVoid (VArgsType Void)))), Map.fromList [("a",TInteger)])
    it "createPrototype Map.empty (Prototype \"hey\" (PrototypeArgs [] Int))" $ do
        createPrototype Map.empty (Prototype "hey" (PrototypeArgs [] Int))
            `shouldBe` (VPrototype (Node (TFunc [TInteger]) (VIdentifier ("hey", Prelude.True))) (Node (TFunc [TInteger]) (VPrototypeArgs [] (Node TInteger (VArgsType Int)))), Map.fromList [])
    it "createPrototype Map.empty (Prototype \"mdr\" (PrototypeArgs [(\"a\",Int),(\"b\",Double)] Double))" $ do
        createPrototype Map.empty (Prototype "mdr" (PrototypeArgs [("a",Int),("b",Double)] Double))
            `shouldBe` (VPrototype (Node (TFunc [TInteger,TDouble,TDouble]) (VIdentifier ("mdr", Prelude.True))) (Node (TFunc [TInteger,TDouble,TDouble]) (VPrototypeArgs [(Node TInteger (VIdentifier ("a", Prelude.True)),Node TInteger (VArgsType Int)),(Node TDouble (VIdentifier ("b", Prelude.True)),Node TDouble (VArgsType Double))] (Node TDouble (VArgsType Double)))), Map.fromList [("a",TInteger),("b",TDouble)])

testCreateDefs :: Spec
testCreateDefs = do
    it "createDefs Map.empty (Defs (Prototype \"plouf\" (PrototypeArgs [(\"a\",Int)] Void)) (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))" $ do
        createDefs Map.empty (Defs (Prototype "plouf" (PrototypeArgs [("a",Int)] Void)) (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))
            `shouldBe` VDefs (Node (TFunc [TInteger,TVoid]) (VPrototype (Node (TFunc [TInteger,TVoid]) (VIdentifier ("plouf", Prelude.True))) (Node (TFunc [TInteger,TVoid]) (VPrototypeArgs [(Node TInteger (VIdentifier ("a", Prelude.True)),Node TInteger (VArgsType Int))] (Node TVoid (VArgsType Void)))))) (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])]))
    it "createDefs Map.empty (Defs (Prototype \"mdr\" (PrototypeArgs [(\"a\", Int), (\"b\", Int)] Double)) (EExpr [Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign, UPostfix (Postfix (PLit (LInt 4)) Nothing)), (Add, UPostfix (Postfix (PId \"b\") Nothing))]]))" $ do
        createDefs Map.empty (Defs (Prototype "mdr" (PrototypeArgs [("a", Int), ("b", Int)] Double)) (EExpr [Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign, UPostfix (Postfix (PLit (LInt 4)) Nothing)), (Add, UPostfix (Postfix (PId "b") Nothing))]]))
            `shouldBe` VDefs (Node (TFunc [TInteger,TInteger,TDouble]) (VPrototype (Node (TFunc [TInteger,TInteger,TDouble]) (VIdentifier ("mdr",Prelude.True))) (Node (TFunc [TInteger,TInteger,TDouble]) (VPrototypeArgs [(Node TInteger (VIdentifier ("a",Prelude.True)),Node TInteger (VArgsType Int)),(Node TInteger (VIdentifier ("b",Prelude.True)),Node TInteger (VArgsType Int))] (Node TDouble (VArgsType Double)))))) (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a",Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 4)))))) (Node TNone VNothing))) (Node TNone VNothing))),(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("b",Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing)))])]))

testCreateKdefs :: Spec
testCreateKdefs = do
    it "createKdefs Map.empty (KDefs (Defs (Prototype \"plouf\" (PrototypeArgs [(\"a\",Int)] Void)) (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]])))" $ do
        createKdefs Map.empty (KDefs (Defs (Prototype "plouf" (PrototypeArgs [("a",Int)] Void)) (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]])))
            `shouldBe` (VKdefs (Node (TFunc [TInteger,TVoid]) (VDefs (Node (TFunc [TInteger,TVoid]) (VPrototype (Node (TFunc [TInteger,TVoid]) (VIdentifier ("plouf", Prelude.True))) (Node (TFunc [TInteger,TVoid]) (VPrototypeArgs [(Node TInteger (VIdentifier ("a", Prelude.True)),Node TInteger (VArgsType Int))] (Node TVoid (VArgsType Void)))))) (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])])))), Map.fromList [("plouf",TFunc [TInteger,TVoid])])
    it "createKdefs Map.empty (KExprs (EExpr [Expr (UPostfix (Postfix (PId \"a\") Nothing)) [(Assign, UPostfix (Postfix (PLit (LInt 1)) Nothing)), (Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))" $ do
        createKdefs Map.empty (KExprs (EExpr [Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign, UPostfix (Postfix (PLit (LInt 1)) Nothing)), (Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]]))
            `shouldBe` (VKdefs (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier ("a", Prelude.True))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))),(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])])), Map.fromList [("a",TInteger)])

testCreateStmt :: Spec
testCreateStmt = do
    it "createStmt Map.empty [KDefs (Defs (Prototype \"test\" (PrototypeArgs [(\"x\",Double)] Double)) (EExpr [Expr (UPostfix (Postfix (PId \"x\") Nothing)) [(Add,UPostfix (Postfix (PLit (LDouble 2.0)) Nothing))]])), KExprs (EExpr [Expr (UPostfix (Postfix (PId \"test\") (Just [Expr (UPostfix (Postfix (PLit (LDouble 5.0)) Nothing)) []]))) [(Sub, UPostfix (Postfix (PLit (LInt 2)) Nothing)), (Mul, UPostfix (Postfix (PLit (LInt 3)) Nothing)), (Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]])]" $ do
        createStmt Map.empty [KDefs (Defs (Prototype "test" (PrototypeArgs [("x",Double)] Double)) (EExpr [Expr (UPostfix (Postfix (PId "x") Nothing)) [(Add,UPostfix (Postfix (PLit (LDouble 2.0)) Nothing))]])), KExprs (EExpr [Expr (UPostfix (Postfix (PId "test") (Just [Expr (UPostfix (Postfix (PLit (LDouble 5.0)) Nothing)) []]))) [(Sub, UPostfix (Postfix (PLit (LInt 2)) Nothing)), (Mul, UPostfix (Postfix (PLit (LInt 3)) Nothing)), (Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]])]
            `shouldBe` (VStmt [Node (TFunc [TDouble,TDouble]) (VKdefs (Node (TFunc [TDouble,TDouble]) (VDefs (Node (TFunc [TDouble,TDouble]) (VPrototype (Node (TFunc [TDouble,TDouble]) (VIdentifier ("test", Prelude.True))) (Node (TFunc [TDouble,TDouble]) (VPrototypeArgs [(Node TDouble (VIdentifier ("x", Prelude.True)),Node TDouble (VArgsType Double))] (Node TDouble (VArgsType Double)))))) (Node TDouble (VExprs [Node TDouble (VExpr (Node TDouble (VUnary (Node TDouble (VPostfix (Node TDouble (VPrimary (Node TDouble (VIdentifier ("x", Prelude.False))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TDouble (VUnary (Node TDouble (VPostfix (Node TDouble (VPrimary (Node TDouble (VLiteral (Node TDouble (VDoubleConst 2.0)))))) (Node TNone VNothing))) (Node TNone VNothing)))])]))))),Node TDouble (VKdefs (Node TDouble (VExprs [Node TDouble (VExpr (Node (TFunc [TDouble,TDouble]) (VUnary (Node (TFunc [TDouble,TDouble]) (VPostfix (Node (TFunc [TDouble,TDouble]) (VPrimary (Node (TFunc [TDouble,TDouble]) (VIdentifier ("test", Prelude.False))))) (Node (TFunc [TDouble,TDouble]) (VCallExpr [Node TDouble (VExpr (Node TDouble (VUnary (Node TDouble (VPostfix (Node TDouble (VPrimary (Node TDouble (VLiteral (Node TDouble (VDoubleConst 5.0)))))) (Node TNone VNothing))) (Node TNone VNothing))) [])])))) (Node TNone VNothing))) [(Node TNone (VBinop Sub),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 2)))))) (Node TNone VNothing))) (Node TNone VNothing))),(Node TNone (VBinop Mul),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 3)))))) (Node TNone VNothing))) (Node TNone VNothing))),(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])])))], Map.fromList [("test",TFunc [TDouble,TDouble])])