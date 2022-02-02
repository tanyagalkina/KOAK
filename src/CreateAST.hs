{-# LANGUAGE LambdaCase #-}
module CreateAST where

-- Import

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map

import Parser

import Data
import ParseCode

-- create AST / EDITABLE

createAST :: Parser AST
createAST = createNTN . createStmt Map.empty <$> parseStmt

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
createLiteral (LInt i) =
    VLiteral (Node TInteger (createDecimalConst i))
createLiteral (LDouble d) =
    VLiteral (Node TDouble (createDoubleConst d))

createLiteralNode :: Value -> Node
createLiteralNode v@(VLiteral (Node TInteger _)) = Node TInteger v
createLiteralNode v@(VLiteral (Node TDouble _)) = Node TDouble v
createLiteralNode _ = Error "Typing of Literal failed"

-- create Identifier

createId :: Identifier -> Value
createId = VIdentifier

createIdNode :: Map Identifier Type -> Value -> Node
createIdNode map v@(VIdentifier s) =
    case Map.lookup s map of
        (Just t) -> Node t v
        Nothing  -> Node TNone v
createIdNode _ _ = Error "Typing of Identifier failed"

-- create Primary

createPrimary :: Map Identifier Type -> Primary -> Value
createPrimary map (PId i) =
    VPrimary (createIdNode map (createId i))
createPrimary _ (PLit l) = VPrimary (createLiteralNode (createLiteral l))
createPrimary map (PExprs es) = VPrimary (createNTN (createExprs map es))

createPrimaryNode :: Value -> Node
createPrimaryNode p@(VPrimary (Node t _)) = Node t p
createPrimaryNode _ = Error "Typing of Primary failed"

-- create Call Expr

createCallExpr :: Map Identifier Type -> CallExpr -> Value
createCallExpr _ [] = VCallExpr [createEmptyNode]
createCallExpr map e = VCallExpr (fmap (createNTN . createExpr map) e)

-- create Postfix

createPostfix :: Map Identifier Type -> Postfix -> Value
createPostfix map (Postfix p Nothing) =
    VPostfix (createPrimaryNode (createPrimary map p))
             createEmptyNode
createPostfix map (Postfix p (Just c)) =
        case primaryNode of
            (Node t@(TFunc _) _) -> VPostfix primaryNode
                                             (Node t (createCallExpr map c))
            _ -> VError "Typing of Postfix failed"
    where
        primaryNode = createPrimaryNode (createPrimary map p)

-- create Unary

createUnary :: Map Identifier Type -> Unary -> Value
createUnary map (Unop uno una) = VUnary (Node TNone (createUnop uno))
                                    (createNTN (createUnary map una))
createUnary map (UPostfix p) = VUnary (createNTN (createPostfix map p))
                                  createEmptyNode

-- create Expr

createExpr :: Map Identifier Type -> Expr -> Value
createExpr map (Expr u bu) =
    VExpr (createNTN (createUnary map u))
          (zip (fmap (Node TNone . createBinop . fst) bu)
               (fmap (createNTN . createUnary map . snd) bu))

-- create For Expr

createForExpr :: Map Identifier Type -> ForExpr -> Value
createForExpr map (ForExpr (i1, e1) (i2, e2) e es) =
    VForExpr (createNTN (createId i1), createNTN (createExpr map e1))
             (createNTN (createId i2), createNTN (createExpr map e2))
             (createNTN (createExpr map e))
             (createNTN (createExprs map es))

-- create If Expr

createIfExpr :: Map Identifier Type -> IfExpr -> Value
createIfExpr map (IfExpr e es Nothing) =
    VIfExpr (createNTN (createExpr map e))
            (createNTN (createExprs map es))
            createEmptyNode
createIfExpr map (IfExpr e es1 (Just es2)) =
    VIfExpr (createNTN (createExpr map e))
            (createNTN (createExprs map es1))
            (createNTN (createExprs map es2))

-- create While Expr

createWhileExpr :: Map Identifier Type -> WhileExpr -> Value
createWhileExpr map (WhileExpr e es) =
    VWhileExpr (createNTN (createExpr map e))
               (createNTN (createExprs map es))

-- create Exprs

createExprs :: Map Identifier Type -> Exprs -> Value
createExprs map (EForExpr f) = VExprs [createNTN (createForExpr map f)]
createExprs map (EWhileExpr w) = VExprs [createNTN (createWhileExpr map w)]
createExprs map (EIfExpr i) = VExprs [createNTN (createIfExpr map i)]
createExprs map (EExpr e) = VExprs (fmap (createNTN . createExpr map) e)

-- create Args Type

createArgsType :: ArgsType -> Value
createArgsType = VArgsType

createArgsTypeNode :: Value -> Node
createArgsTypeNode v@(VArgsType Int) = Node TInteger v
createArgsTypeNode v@(VArgsType Double) = Node TDouble v
createArgsTypeNode v@(VArgsType Void) = Node TVoid v
createArgsTypeNode _ = Error "Typing of Args Type failed"

-- create Prototype Args

createPrototypeArgs :: PrototypeArgs -> Value
createPrototypeArgs (PrototypeArgs ia a) =
    VPrototypeArgs
        (fmap (\(i, a) ->
                let argsTypeNode = createArgsTypeNode (createArgsType a)
                in (Node (getNodeType argsTypeNode) (createId i), argsTypeNode))
              ia)
        (createArgsTypeNode (createArgsType a))

createPrototypeArgsNode :: Value -> Node
createPrototypeArgsNode v@(VPrototypeArgs args return) =
    case turnListOfTypeInFunc (fmap snd args ++ [return]) of
        TError s -> Error s
        t -> Node t v
createPrototypeArgsNode _ = Error "Typing of Prototype Args failed"

-- create Prototype

createPrototype :: Prototype -> Value
createPrototype (Prototype i pa) =
    VPrototype (Node (getNodeType protoArgsNode) (createId i))
               protoArgsNode
    where
        protoArgsNode = createPrototypeArgsNode (createPrototypeArgs pa)

createPrototypeNode :: Value -> Node
createPrototypeNode v@(VPrototype (Node t _) _) = Node t v
createPrototypeNode _ = Error "Typing of Prototype failed"

-- create Defs

createDefs :: Map Identifier Type -> Defs -> Value
createDefs map (Defs p es) = VDefs (createPrototypeNode (createPrototype p))
                                   (createNTN (createExprs map es))

-- create Kdefs

createKdefs :: Map Identifier Type -> Kdefs -> Value
createKdefs map (KDefs d) = VKdefs (createNTN (createDefs map d))
createKdefs map (KExprs es) = VKdefs (createNTN (createExprs map es))

-- create Stmt

createStmt :: Map Identifier Type -> Stmt -> Value
createStmt map k = VStmt (fmap (createNTN . createKdefs map) k)

-- turn list of type into function type

turnListOfTypeInFunc :: [Node] -> Type
turnListOfTypeInFunc [] = TFunc []
turnListOfTypeInFunc ((Node t (VArgsType _)):xs) =
    if t `elem` [TInteger, TDouble, TVoid]
    then addTypeToList t (turnListOfTypeInFunc xs)
    else TError "Typing of Prototype Args failed"
turnListOfTypeInFunc (_:xs) = TError "Typing of Prototype Args failed"

addTypeToList :: Type -> Type -> Type
addTypeToList t rest = case rest of
    TFunc list -> TFunc (t : list)
    TInteger -> TFunc [t, TInteger]
    TDouble  -> TFunc [t, TDouble]
    TVoid -> TFunc [t, TVoid]
    _ -> TError "Typing of Prototype Args failed"

-- get Node Type

getNodeType :: Node -> Type 
getNodeType (Node t _) = t
getNodeType (Error s) = TError s