{-# LANGUAGE LambdaCase #-}
module CreateAST where

-- Import

import Control.Applicative

import Parser

import Data
import ParseCode

-- create AST / EDITABLE

createAST :: Parser AST
createAST = createNTN . createStmt <$> parseStmt

-- create Basic Nodes

createNoTypeNode :: Value -> Node
createNoTypeNode = Node TUndefine

createNTN :: Value -> Node
createNTN = createNoTypeNode

createEmptyNode :: Node
createEmptyNode = Node TUndefine VNothing

-- create Decimal Const

createDecimalConst :: DecimalConst -> Value
createDecimalConst = VDecimalConst

-- create Double Const

createDoubleConst :: DoubleConst -> Value
createDoubleConst = VDoubleConst

-- create Unop

createUnop :: Unop -> Value
createUnop = VUnop

-- create Binop

createBinop :: Binop -> Value
createBinop = VBinop

-- create Literal

createLiteral :: Literal -> Value
createLiteral (LInt i) = VLiteral (createNTN (createDecimalConst i))
createLiteral (LDouble d) = VLiteral (createNTN (createDoubleConst d))

-- create Identifier

createIdentifier :: Identifier -> Value
createIdentifier = VIdentifier

-- create Primary

createPrimary :: Primary -> Value
createPrimary (PId i) = VPrimary (createNTN (createIdentifier i))
createPrimary (PLit l) = VPrimary (createNTN (createLiteral l))
createPrimary (PExprs es) = VPrimary (createNTN (createExprs es))

-- create Call Expr

createCallExpr :: CallExpr -> Value
createCallExpr [] = VCallExpr [createEmptyNode]
createCallExpr e = VCallExpr (fmap (createNTN . createExpr) e)

-- create Postfix

createPostfix :: Postfix -> Value
createPostfix (Postfix p Nothing) = VPostfix (createNTN (createPrimary p))
                                             createEmptyNode
createPostfix (Postfix p (Just c)) = VPostfix (createNTN (createPrimary p))
                                              (createNTN (createCallExpr c))

-- create Unary

createUnary :: Unary -> Value
createUnary (Unop uno una) = VUnary (createNTN (createUnop uno))
                                    (createNTN (createUnary una))
createUnary (UPostfix p) = VUnary (createNTN (createPostfix p))
                                  createEmptyNode

-- create Expr

createExpr :: Expr -> Value
createExpr (Expr u bu) = VExpr (createNTN (createUnary u))
                               (zip (fmap (createNTN . createBinop . fst) bu)
                                    (fmap (createNTN . createUnary . snd) bu))

-- create For Expr

createForExpr :: ForExpr -> Value
createForExpr (ForExpr (i1, e1) (i2, e2) e es) =
    VForExpr (createNTN (createIdentifier i1), createNTN (createExpr e1))
             (createNTN (createIdentifier i2), createNTN (createExpr e2))
             (createNTN (createExpr e))
             (createNTN (createExprs es))

-- create If Expr

createIfExpr :: IfExpr -> Value
createIfExpr (IfExpr e es Nothing) =
    VIfExpr (createNTN (createExpr e))
            (createNTN (createExprs es))
            createEmptyNode
createIfExpr (IfExpr e es1 (Just es2)) =
    VIfExpr (createNTN (createExpr e))
            (createNTN (createExprs es1))
            (createNTN (createExprs es2))

-- create While Expr

createWhileExpr :: WhileExpr -> Value
createWhileExpr (WhileExpr e es) =
    VWhileExpr (createNTN (createExpr e))
               (createNTN (createExprs es))

-- create Exprs

createExprs :: Exprs -> Value
createExprs (EForExpr f) = VExprs [createNTN (createForExpr f)]
createExprs (EWhileExpr w) = VExprs [createNTN (createWhileExpr w)]
createExprs (EIfExpr i) = VExprs [createNTN (createIfExpr i)]
createExprs (EExprs e) = VExprs (fmap (createNTN . createExpr) e)

-- create Args Type

createArgsType :: ArgsType -> Value
createArgsType = VArgsType

-- create Prototype Args

createPrototypeArgs :: PrototypeArgs -> Value
createPrototypeArgs (PrototypeArgs ia a) =
    VPrototypeArgs (zip (fmap (createNTN . createIdentifier . fst) ia)
                        (fmap (createNTN . createArgsType . snd) ia))
                   (createNTN (createArgsType a))

-- create Prototype

createPrototype :: Prototype -> Value
createPrototype (Prototype i pa) =
    VPrototype (createNTN (createIdentifier i))
               (createNTN (createPrototypeArgs pa))

-- create Defs

createDefs :: Defs -> Value
createDefs (Defs p es) = VDefs (createNTN (createPrototype p))
                               (createNTN (createExprs es))

-- create Kdefs

createKdefs :: Kdefs -> Value
createKdefs (KDefs d) = VKdefs (createNTN (createDefs d))
createKdefs (KExprs es) = VKdefs (createNTN (createExprs es))

-- create Stmt

createStmt :: Stmt -> Value
createStmt k = VStmt (fmap (createNTN . createKdefs) k)