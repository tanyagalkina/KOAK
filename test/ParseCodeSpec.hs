module ParseCodeSpec (spec) where

import Test.Hspec

import Parser

import Data
import ParseCode

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
    describe "parseForExpr"           testParseForExpr
    describe "parseIfExpr"            testParseIfExpr
    describe "parseWhileExpr"         testParseWhileExpr
    describe "parseExprs"             testParseExprs
    describe "parseArgsType"          testParseArgsType
    describe "parsePrototypeArgs"     testParsePrototypeArgs
    describe "parsePrototype"         testParsePrototype
    describe "parseDefs"              testParseDefs
    describe "parseKdefs"             testParseKdefs
    describe "parseStmt"              testParseStmt 

testParseUnop :: Spec
testParseUnop = do
    it "runParser parseUnop \"-\"" $ do
        runParser parseUnop "-" `shouldBe` Just (Minus, "")
    it "runParser parseUnop \"!\"" $ do
        runParser parseUnop "!" `shouldBe` Just (Not, "")
    it "runParser parseUnop \"-  \"" $ do
        runParser parseUnop "-  " `shouldBe` Just (Minus, "  ")
    it "runParser parseUnop \"!  \"" $ do
        runParser parseUnop "!  " `shouldBe` Just (Not, "  ")
    it "runParser parseUnop \"a\"" $ do
        runParser parseUnop "a" `shouldBe` Nothing

testParseBinop :: Spec
testParseBinop = do
    it "runParser parseBinop \"* \"" $ do
        runParser parseBinop "* " `shouldBe` Just (Mul, " ")
    it "runParser parseBinop \"+ \"" $ do
        runParser parseBinop "+ " `shouldBe` Just (Add, " ")
    it "runParser parseBinop \"/ \"" $ do
        runParser parseBinop "/ " `shouldBe` Just (Div, " ")
    it "runParser parseBinop \"- \"" $ do
        runParser parseBinop "- " `shouldBe` Just (Sub, " ")
    it "runParser parseBinop \"== \"" $ do
        runParser parseBinop "== " `shouldBe` Just (Eq, " ")
    it "runParser parseBinop \"!= \"" $ do
        runParser parseBinop "!= " `shouldBe` Just (Neq, " ")
    it "runParser parseBinop \"< \"" $ do
        runParser parseBinop "< " `shouldBe` Just (Lt, " ")
    it "runParser parseBinop \"> \"" $ do
        runParser parseBinop "> " `shouldBe` Just (Gt, " ")
    it "runParser parseBinop \"= \"" $ do
        runParser parseBinop "= " `shouldBe` Just (Assign, " ")
    it "runParser parseBinop \"a\"" $ do
        runParser parseBinop "a" `shouldBe` Nothing

testParseDoubleConst :: Spec
testParseDoubleConst = do
    it "runParser parseDoubleConst \"10.4  \"" $ do
        runParser parseDoubleConst "10.4  " `shouldBe` Just (10.4, "  ")
    it "runParser parseDoubleConst \".4  \"" $ do
        runParser parseDoubleConst ".4  " `shouldBe` Just (0.4, "  ")
    it "runParser parseDoubleConst \"1.4  \"" $ do
        runParser parseDoubleConst "1.4  " `shouldBe` Just (1.4, "  ")
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
        runParser parseDecimalConst "10  " `shouldBe` Just (10, "  ")
    it "runParser parseDecimalConst \"a\"" $ do
        runParser parseDecimalConst "a" `shouldBe` Nothing

testParseLiteral :: Spec
testParseLiteral = do
    it "runParser parseLiteral \"10.4  \"" $ do
        runParser parseLiteral "10.4  " `shouldBe` Just (LDouble 10.4, "  ")
    it "runParser parseLiteral \".4  \"" $ do
        runParser parseLiteral ".4  " `shouldBe` Just (LDouble 0.4, "  ")
    it "runParser parseLiteral \"10  \"" $ do
        runParser parseLiteral "10  " `shouldBe` Just (LInt 10, "  ")
    it "runParser parseLiteral \"a\"" $ do
        runParser parseLiteral "a" `shouldBe` Nothing

testParseIdentifier :: Spec
testParseIdentifier = do
    it "runParser parseIdentifier \"Patrick  \"" $ do
        runParser parseIdentifier "Patrick  " `shouldBe` Just ("Patrick", "  ")
    it "runParser parseIdentifier \"   Patrick  \"" $ do
        runParser parseIdentifier "   Patrick  " `shouldBe` Just ("Patrick", "  ")
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
        runParser parsePrimary "12.3  " `shouldBe` Just (PLit (LDouble 12.3), "  ")
    it "runParser parsePrimary \"12\"" $ do
        runParser parsePrimary "12" `shouldBe` Just (PLit (LInt 12), "")
    it "runParser parsePrimary \"guy  \"" $ do
        runParser parsePrimary "guy  " `shouldBe` Just (PId "guy", "  ")
    it "runParser parsePrimary \"(1 + 1)\"" $ do
        runParser parsePrimary "(1 + 1)"
        `shouldBe` Just (PExprs (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]]),"")
    it "runParser parsePrimary \" \"" $ do
        runParser parsePrimary " " `shouldBe` Nothing

testParseCallExpr :: Spec
testParseCallExpr = do
    it "runParser parseCallExpr \"(12  +  Patrick  )\"" $ do
        runParser parseCallExpr "(12  +  Patrick  )"
            `shouldBe` Just ([Expr (UPostfix (Postfix (PLit (LInt 12)) Nothing)) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))]], "")
    it "runParser parseCallExpr \"(  12  +  Patrick  )\"" $ do
        runParser parseCallExpr "(  12  +  Patrick  )"
            `shouldBe` Just ([Expr (UPostfix (Postfix (PLit (LInt 12)) Nothing)) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))]], "")
    it "runParser parseCallExpr \"(- 12  +  Patrick  ,   1 / 2)\"" $ do
        runParser parseCallExpr "(- 12  +  Patrick  ,   1 / 2)"
        `shouldBe` Just ([Expr (Unop Minus (UPostfix (Postfix (PLit (LInt 12)) Nothing))) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))]
                         ,Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Div, UPostfix (Postfix (PLit (LInt 2)) Nothing))]], "")
    it "runParser parseCallExpr \"(bonjour == aurevoir  )  \"" $ do
        runParser parseCallExpr "(bonjour == aurevoir  )  "
        `shouldBe` Just ([Expr (UPostfix (Postfix (PId "bonjour") Nothing)) [(Eq, UPostfix (Postfix (PId "aurevoir") Nothing))]], "  ")
    it "runParser parseCallExpr \"(guy  )\"" $ do
        runParser parseCallExpr "(guy  )" `shouldBe` Just ([Expr (UPostfix (Postfix (PId "guy") Nothing)) []], "")
    it "runParser parseCallExpr \"1 = 2\"" $ do
        runParser parseCallExpr "1 = 2" `shouldBe` Nothing
    it "runParser parseCallExpr \"()\"" $ do
        runParser parseCallExpr "()" `shouldBe` Just ([], "")
    it "runParser parseCallExpr \" \"" $ do
        runParser parseCallExpr " " `shouldBe` Nothing

testParsePostfix :: Spec
testParsePostfix = do
    it "runParser parsePostfix \"12.3  \"" $ do
        runParser parsePostfix "12.3  " `shouldBe` Just (Postfix (PLit (LDouble 12.3)) Nothing, "  ")
    it "runParser parsePostfix \"12\"" $ do
        runParser parsePostfix "12" `shouldBe` Just (Postfix (PLit (LInt 12)) Nothing, "")
    it "runParser parsePostfix \"guy  \"" $ do
        runParser parsePostfix "guy  " `shouldBe` Just (Postfix (PId "guy") Nothing, "  ")
    it "runParser parsePostfix \"guy()\"" $ do
        runParser parsePostfix "guy()" `shouldBe` Just (Postfix (PId "guy") (Just []), "")
    it "runParser parsePostfix \"guy(1 + 1)\"" $ do
        runParser parsePostfix "guy(1 + 1)"
        `shouldBe` Just (Postfix (PId "guy") (Just [Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))]]), "")
    it "runParser parsePostfix \"guy  ()\"" $ do
        runParser parsePostfix "guy  ()" `shouldBe` Just (Postfix (PId "guy") Nothing, "  ()")
    it "runParser parsePostfix \"()\"" $ do
        runParser parsePostfix "()" `shouldBe` Nothing
    it "runParser parsePostfix \" \"" $ do
        runParser parsePostfix " " `shouldBe` Nothing

testParseUnary :: Spec
testParseUnary = do
    it "runParser parseUnary \"12.3  \"" $ do
        runParser parseUnary "12.3  " `shouldBe` Just (UPostfix (Postfix (PLit (LDouble 12.3)) Nothing), "  ")
    it "runParser parseUnary \"12\"" $ do
        runParser parseUnary "12" `shouldBe` Just (UPostfix (Postfix (PLit (LInt 12)) Nothing), "")
    it "runParser parseUnary \"guy  \"" $ do
        runParser parseUnary "guy  " `shouldBe` Just (UPostfix (Postfix (PId "guy") Nothing), "  ")
    it "runParser parseUnary \"-12.3  \"" $ do
        runParser parseUnary "-12.3  " `shouldBe` Just (Unop Minus (UPostfix (Postfix (PLit (LDouble 12.3)) Nothing)), "  ")
    it "runParser parseUnary \"- plouf\"" $ do
        runParser parseUnary "- plouf" `shouldBe` Just (Unop Minus (UPostfix (Postfix (PId "plouf") Nothing)), "")
    it "runParser parseUnary \"!   guy  \"" $ do
        runParser parseUnary "!   guy  " `shouldBe` Just (Unop Not (UPostfix (Postfix (PId "guy") Nothing)), "  ")
    it "runParser parseUnary \" \"" $ do
        runParser parseUnary " " `shouldBe` Nothing

testParseExpr :: Spec
testParseExpr = do
    it "runParser parseExpr \"12  +  Patrick  \"" $ do
        runParser parseExpr "12  +  Patrick  "
            `shouldBe` Just (Expr (UPostfix (Postfix (PLit (LInt 12)) Nothing)) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))], "  ")
    it "runParser parseExpr \"1 + 1 + 1 \"" $ do
        runParser parseExpr "1 + 1 + 1 "
            `shouldBe` Just (Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing)),(Add, UPostfix (Postfix (PLit (LInt 1)) Nothing))], " ")
    it "runParser parseExpr \"- 12  +  Patrick  \"" $ do
        runParser parseExpr "- 12  +  Patrick  "
        `shouldBe` Just (Expr (Unop Minus (UPostfix (Postfix (PLit (LInt 12)) Nothing))) [(Add, UPostfix (Postfix (PId "Patrick") Nothing))], "  ")
    it "runParser parseExpr \"bonjour == aurevoir    \"" $ do
        runParser parseExpr "bonjour == aurevoir    "
        `shouldBe` Just (Expr (UPostfix (Postfix (PId "bonjour") Nothing)) [(Eq, UPostfix (Postfix (PId "aurevoir") Nothing))], "    ")
    it "runParser parseExpr \"guy  \"" $ do
        runParser parseExpr "guy  " `shouldBe` Just (Expr (UPostfix (Postfix (PId "guy") Nothing)) [], "  ")
    it "runParser parseExpr \"+ +\"" $ do
        runParser parseExpr "+ +" `shouldBe` Nothing
    it "runParser parseExpr \" \"" $ do
        runParser parseExpr " " `shouldBe` Nothing

testParseForExpr :: Spec
testParseForExpr = do
    it "runParser parseForExpr \"for a=1, a<3, a = a + 1 in b = b + 1\"" $ do
        runParser parseForExpr "for a=1, a<3, a = a + 1 in b = b + 1"
            `shouldBe` Just (ForExpr ("a",Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [])
                                     ("a",Expr (UPostfix (Postfix (PLit (LInt 3)) Nothing)) [])
                                     (Expr (UPostfix (Postfix (PId "a") Nothing))
                                        [(Assign,UPostfix (Postfix (PId "a") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))])
                                     (EExpr
                                        [Expr (UPostfix (Postfix (PId "b") Nothing))
                                            [(Assign,UPostfix (Postfix (PId "b") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]]),"")
    it "runParser parseForExpr \"for a=1, a<3, a = a + 1 inb = b + 1\"" $ do
        runParser parseForExpr "for a=1, a<3, a = a + 1 inb = b + 1" `shouldBe` Nothing
    it "runParser parseForExpr \"for a=1, a<3, a = a + 1in b = b + 1\"" $ do
        runParser parseForExpr "for a=1, a<3, a = a + 1in b = b + 1" `shouldBe` Nothing
    it "runParser parseForExpr \"fora=1, a<3, a = a + 1 inb = b + 1\"" $ do
        runParser parseForExpr "fora=1, a<3, a = a + 1 inb = b + 1" `shouldBe` Nothing
    it "runParser parseForExpr \" \"" $ do
        runParser parseForExpr " " `shouldBe` Nothing

testParseIfExpr :: Spec
testParseIfExpr = do
    it "runParser parseIfExpr \"if a = b then c = b\"" $ do
        runParser parseIfExpr "if a = b then c = b"
            `shouldBe` Just (IfExpr (Expr (UPostfix (Postfix (PId "a") Nothing))
                                          [(Assign,UPostfix (Postfix (PId "b") Nothing))])
                                    (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing))
                                            [(Assign,UPostfix (Postfix (PId "b") Nothing))]]) Nothing,"")
    it "runParser parseIfExpr \"if a = b then c = b else c = a\"" $ do
        runParser parseIfExpr "if a = b then c = b else c = a"
            `shouldBe` Just (IfExpr (Expr (UPostfix (Postfix (PId "a") Nothing))
                                          [(Assign,UPostfix (Postfix (PId "b") Nothing))])
                                    (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing))
                                                  [(Assign,UPostfix (Postfix (PId "b") Nothing))]])
                                    (Just (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing))
                                                        [(Assign,UPostfix (Postfix (PId "a") Nothing))]])),"")
    it "runParser parseIfExpr \"ifa = b then c = b else c = a\"" $ do
        runParser parseIfExpr "ifa = b then c = b else c = a" `shouldBe` Nothing
    it "runParser parseIfExpr \"if a = bthen c = b else c = a\"" $ do
        runParser parseIfExpr "if a = bthen c = b else c = a" `shouldBe` Nothing
    it "runParser parseIfExpr \"if a = b thenc = b else c = a\"" $ do
        runParser parseIfExpr "if a = b thenc = b else c = a" `shouldBe` Nothing
    it "runParser parseIfExpr \"if a = b then c = belse c = a\"" $ do
        runParser parseIfExpr "if a = b then c = belse c = a" `shouldBe` Just (IfExpr (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "belse") Nothing))]]) Nothing," c = a")
    it "runParser parseIfExpr \"if a = b then c = b elsec = a\"" $ do
        runParser parseIfExpr "if a = b then c = b elsec = a" `shouldBe` Just (IfExpr (Expr (UPostfix (Postfix (PId "a") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]) (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing)) [(Assign,UPostfix (Postfix (PId "b") Nothing))]]) Nothing," elsec = a")
    it "runParser parseIfExpr \" \"" $ do
        runParser parseIfExpr " " `shouldBe` Nothing

testParseWhileExpr :: Spec
testParseWhileExpr = do
    it "runParser parseWhileExpr \"while a == b do c = b\"" $ do
        runParser parseWhileExpr "while a == b do c = b"
            `shouldBe` Just (WhileExpr (Expr (UPostfix (Postfix (PId "a") Nothing))
                                             [(Eq,UPostfix (Postfix (PId "b") Nothing))])
                                       (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing))
                                                     [(Assign,UPostfix (Postfix (PId "b") Nothing))]]),"")
    it "runParser parseWhileExpr \"whilea == b then c = b\"" $ do
        runParser parseWhileExpr "whilea == b then c = b" `shouldBe` Nothing
    it "runParser parseWhileExpr \"whilea == b do c = b\"" $ do
        runParser parseWhileExpr "whilea == b do c = b" `shouldBe` Nothing
    it "runParser parseWhileExpr \"while a == bdo c = b\"" $ do
        runParser parseWhileExpr "while a == bdo c = b" `shouldBe` Nothing
    it "runParser parseWhileExpr \"while a == b doc = b\"" $ do
        runParser parseWhileExpr "while a == b doc = b" `shouldBe` Nothing
    it "runParser parseWhileExpr \" \"" $ do
        runParser parseWhileExpr " " `shouldBe` Nothing

testParseExprs :: Spec
testParseExprs = do
    it "runParser parseExprs \"while a == b do c = b\"" $ do
        runParser parseExprs "while a == b do c = b"
            `shouldBe` Just (EWhileExpr (WhileExpr (Expr (UPostfix (Postfix (PId "a") Nothing))
                                                         [(Eq,UPostfix (Postfix (PId "b") Nothing))])
                                                   (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing))
                                                                 [(Assign,UPostfix (Postfix (PId "b") Nothing))]])),"")
    it "runParser parseExprs \"if a = b then c = b else c = a\"" $ do
        runParser parseExprs "if a = b then c = b else c = a"
            `shouldBe` Just (EIfExpr (IfExpr (Expr (UPostfix (Postfix (PId "a") Nothing))
                                                   [(Assign,UPostfix (Postfix (PId "b") Nothing))])
                                             (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing))
                                                           [(Assign,UPostfix (Postfix (PId "b") Nothing))]])
                                             (Just (EExpr [Expr (UPostfix (Postfix (PId "c") Nothing))
                                                                 [(Assign,UPostfix (Postfix (PId "a") Nothing))]]))),"")
    it "runParser parseExprs \"for a=1, a<3, a = a + 1 in b = b + 1\"" $ do
        runParser parseExprs "for a=1, a<3, a = a + 1 in b = b + 1"
            `shouldBe` Just (EForExpr (ForExpr ("a",Expr (UPostfix (Postfix (PLit (LInt 1)) Nothing)) [])
                                               ("a",Expr (UPostfix (Postfix (PLit (LInt 3)) Nothing)) [])
                                               (Expr (UPostfix (Postfix (PId "a") Nothing))
                                                     [(Assign,UPostfix (Postfix (PId "a") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))])
                                               (EExpr [Expr (UPostfix (Postfix (PId "b") Nothing))
                                                             [(Assign,UPostfix (Postfix (PId "b") Nothing)),(Add,UPostfix (Postfix (PLit (LInt 1)) Nothing))]])),"")
    it "runParser parseExprs \"a + 1 : a + 2\"" $ do
        runParser parseExprs "a + 1 : a + 2"
            `shouldBe` Just (EExpr [Expr (UPostfix (Postfix (PId "a")
                                                             Nothing))
                                          [(Add, UPostfix (Postfix (PLit (LInt 1))
                                                                   Nothing))],
                                    Expr (UPostfix (Postfix (PId "a")
                                                            Nothing))
                                         [(Add, UPostfix (Postfix (PLit (LInt 2))
                                                                  Nothing))]],"")
    it "runParser parseExprs \" \"" $ do
        runParser parseExprs " " `shouldBe` Nothing

testParseArgsType :: Spec
testParseArgsType = do
    it "runParser parseArgsType \"int  \"" $ do
        runParser parseArgsType "int  " `shouldBe` Just (Int, "  ")
    it "runParser parseArgsType \"double\"" $ do
        runParser parseArgsType "double" `shouldBe` Just (Double, "")
    it "runParser parseArgsType \"void  \"" $ do
        runParser parseArgsType "void  " `shouldBe` Just (Void, "  ")
    it "runParser parseArgsType \"patate\"" $ do
        runParser parseArgsType "patate" `shouldBe` Nothing
    it "runParser parseArgsType \" \"" $ do
        runParser parseArgsType " " `shouldBe` Nothing

testParsePrototypeArgs :: Spec
testParsePrototypeArgs = do
    it "runParser parsePrototypeArgs \"(a:int):void\"" $ do
        runParser parsePrototypeArgs "(a:int):void" `shouldBe` Just (PrototypeArgs [("a",Int)] Void,"")
    it "runParser parsePrototypeArgs \"(   a   :   int   ):void\"" $ do
        runParser parsePrototypeArgs "(   a   :   int   ):void" `shouldBe` Just (PrototypeArgs [("a",Int)] Void,"")
    it "runParser parsePrototypeArgs \"():int\"" $ do
        runParser parsePrototypeArgs "():int" `shouldBe` Just (PrototypeArgs [] Int,"")
    it "runParser parsePrototypeArgs \"(a:int , b:double)  :   double\"" $ do
        runParser parsePrototypeArgs "(a:int , b:double)  :   double" `shouldBe` Just (PrototypeArgs [("a",Int),("b",Double)] Double,"")
    it "runParser parsePrototypeArgs \"(a:void)\"" $ do
        runParser parsePrototypeArgs "(a:void)" `shouldBe` Nothing
    it "runParser parsePrototypeArgs \" \"" $ do
        runParser parsePrototypeArgs " " `shouldBe` Nothing

testParsePrototype :: Spec
testParsePrototype = do
    it "runParser parsePrototype \"plouf(a:int):void\"" $ do
        runParser parsePrototype "plouf(a:int):void" `shouldBe` Just (Prototype "plouf" (PrototypeArgs [("a",Int)] Void),"")
    it "runParser parsePrototype \"hey  ():int\"" $ do
        runParser parsePrototype "hey  ():int" `shouldBe` Just (Prototype "hey" (PrototypeArgs [] Int),"")
    it "runParser parsePrototype \"mdr   (a:int , b:double)  :   double\"" $ do
        runParser parsePrototype "mdr   (a:int , b:double)  :   double" `shouldBe` Just (Prototype "mdr" (PrototypeArgs [("a",Int),("b",Double)] Double),"")
    it "runParser parsePrototype \"(a:void):void\"" $ do
        runParser parsePrototype "(a:void):void" `shouldBe` Nothing
    it "runParser parsePrototype \" \"" $ do
        runParser parsePrototype " " `shouldBe` Nothing

testParseDefs :: Spec
testParseDefs = do
    it "runParser parseDefs \"plouf(a:int):void 1 + 1\"" $ do
        runParser parseDefs "plouf(a:int):void 1 + 1"
            `shouldBe` Just (Defs (Prototype "plouf"
                                             (PrototypeArgs [("a",Int)]
                                                            Void))
                                  (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1))
                                                                   Nothing))
                                                [(Add, UPostfix (Postfix (PLit (LInt 1))
                                                                Nothing))]]),"")
    it "runParser parseDefs \"mdr   (a:int , b:double)  :   double a = 4 + b\"" $ do
        runParser parseDefs "mdr   (a:int , b:double)  :   double a = 4 + b"
            `shouldBe` Just (Defs (Prototype "mdr"
                                             (PrototypeArgs [("a", Int), ("b", Double)]
                                                            Double))
                                  (EExpr [Expr (UPostfix (Postfix (PId "a")
                                                                   Nothing))
                                                [(Assign, UPostfix (Postfix (PLit (LInt 4))
                                                                            Nothing)),
                                                (Add, UPostfix (Postfix (PId "b")
                                                                        Nothing))]]),"")
    it "runParser parseDefs \" \"" $ do
        runParser parseDefs " " `shouldBe` Nothing

testParseKdefs :: Spec
testParseKdefs = do
    it "runParser parseKdefs \"def plouf(a:int):void 1 + 1;\"" $ do
        runParser parseKdefs "def plouf(a:int):void 1 + 1;"
            `shouldBe` Just (KDefs (Defs (Prototype "plouf"
                                                    (PrototypeArgs [("a",Int)]
                                                                   Void))
                                         (EExpr [Expr (UPostfix (Postfix (PLit (LInt 1))
                                                                               Nothing))
                                                       [(Add, UPostfix (Postfix (PLit (LInt 1))
                                                                                Nothing))]])),"")
    it "runParser parseKdefs \"a = 1 + 1;\"" $ do
        runParser parseKdefs "a = 1 + 1;"
            `shouldBe` Just (KExprs (EExpr [Expr (UPostfix (Postfix (PId "a")
                                                                     Nothing))
                                                  [(Assign, UPostfix (Postfix (PLit (LInt 1))
                                                                              Nothing)),
                                                  (Add, UPostfix (Postfix (PLit (LInt 1))
                                                                          Nothing))]]),"")
    it "runParser parseKdefs \" \"" $ do
        runParser parseKdefs " " `shouldBe` Nothing

testParseStmt :: Spec
testParseStmt = do
    it "runParser parseStmt \"def test ( x : double ) : double x + 2.0; test(5.0) - 2 * 3 + 1;\"" $ do
        runParser parseStmt "def test ( x : double ) : double x + 2.0; test(5.0) - 2 * 3 + 1;"
            `shouldBe` Just ([KDefs (Defs (Prototype "test"
                                                     (PrototypeArgs [("x",Double)]
                                                                    Double))
                                          (EExpr [Expr (UPostfix (Postfix (PId "x")
                                                                           Nothing))
                                                        [(Add,UPostfix (Postfix (PLit (LDouble 2.0))
                                                                                Nothing))]])),
                             KExprs (EExpr [Expr (UPostfix (Postfix (PId "test")
                                                                     (Just [Expr (UPostfix (Postfix (PLit (LDouble 5.0))
                                                                                                    Nothing)) []])))
                                                  [(Sub, UPostfix (Postfix (PLit (LInt 2))
                                                                           Nothing)),
                                                  (Mul, UPostfix (Postfix (PLit (LInt 3))
                                                                          Nothing)),
                                                  (Add, UPostfix (Postfix (PLit (LInt 1))
                                                                          Nothing))]])],"")
    it "runParser parseStmt \" \"" $ do
        runParser parseStmt " " `shouldBe` Nothing