{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
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
createAST = (\s -> let (stmt, map) = createStmt Map.empty s
                   in  Node (TError (show map)) stmt) <$> parseStmt

-- create Basic Nodes

createNoTypeNode :: Value -> Node
createNoTypeNode = Node TUndefine

createNTN :: Value -> Node
createNTN = createNoTypeNode

createEmptyNode :: Node
createEmptyNode = Node TNone VNothing

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

createIdNode :: TypedId -> Value -> Node
createIdNode ti v@(VIdentifier s) =
    case Map.lookup s ti of
        (Just t) -> Node t v
        Nothing  -> Node TNone v
createIdNode _ _ = Error "Typing of Identifier failed"

-- create Primary

createPrimary :: TypedId -> Primary -> Value
createPrimary ti (PId i) =
    VPrimary (createIdNode ti (createId i))
createPrimary _ (PLit l) = VPrimary (createLiteralNode (createLiteral l))
createPrimary ti (PExprs es) = VPrimary (createExprsNode (createExprs ti es))

createPrimaryNode :: Value -> Node
createPrimaryNode p@(VPrimary (Node t _)) = Node t p
createPrimaryNode _ = Error "Typing of Primary failed"

-- create Call Expr

createCallExpr :: TypedId -> CallExpr -> Value
createCallExpr _ [] = VCallExpr [createEmptyNode]
createCallExpr ti e = VCallExpr (fmap (createExprNode . createExpr ti) e)

-- create Postfix

createPostfix :: TypedId -> Postfix -> Value
createPostfix ti (Postfix p Nothing) =
    VPostfix (createPrimaryNode (createPrimary ti p))
             createEmptyNode
createPostfix ti (Postfix p (Just c)) =
        case primaryNode of
            (Node t@(TFunc _) _) -> VPostfix primaryNode
                                             (Node t (createCallExpr ti c))
            _ -> VError "Typing of Postfix failed"
    where
        primaryNode = createPrimaryNode (createPrimary ti p)

createPostfixNode :: Value -> Node
createPostfixNode v@(VPostfix (Node t _) _) = Node t v
createPostfixNode _ = Error "Typing of Postfix failed"

-- create Unary

createUnary :: TypedId -> Unary -> Value
createUnary ti (Unop uno una) = VUnary (Node TNone (createUnop uno))
                                        (createUnaryNode (createUnary ti una))
createUnary ti (UPostfix p) = VUnary (createPostfixNode (createPostfix ti p))
                                      createEmptyNode

createUnaryNode :: Value -> Node
createUnaryNode v@(VUnary (Node t (VPostfix _ _)) _) = Node t v
createUnaryNode v@(VUnary _ (Node t (VUnary _ _))) = Node t v
createUnaryNode _ = Error "Typing of Unary failed"

-- create Expr

createExpr :: TypedId -> Expr -> Value
createExpr ti (Expr u bu) = VExpr unaryNode buNode
    where
        buNode = reverse $ applyAssignType (reverse
                    (zip (fmap (Node TNone . createBinop . fst) bu)
                         (fmap (createUnaryNode . createUnary ti . snd) bu)))
        unaryNode = case (buNode, createUnaryNode (createUnary ti u)) of
            ([], uN) -> uN
            (x:_, uN) -> applyAssignTypeToFirst x uN
        
applyAssignType :: [(Node, Node)] -> [(Node, Node)]
applyAssignType (bu@(Node _ (VBinop Assign), Node t _):bu'@(b, Node t' v):bus) =
    case t' of
        TNone -> bu : (b, Node t v)
                    : tail (applyAssignType ((b, Node t v) : bus))
        _ -> bu : bu' : tail (applyAssignType (bu' : bus))
applyAssignType (bu:bus) = bu : applyAssignType bus
applyAssignType [] = []

applyAssignTypeToFirst :: (Node, Node) -> Node -> Node
applyAssignTypeToFirst (Node _ (VBinop Assign), Node t _) (Node t' v) =
    case t' of
        TNone -> Node t v
        _ -> Node t' v
applyAssignTypeToFirst _ u = u

createExprNode :: Value -> Node
createExprNode v@(VExpr first list) =
    Node (getExprType (getNodeType first : fmap (getNodeType . snd) list)) v
createExprNode _ = Error "Typing of Expr failed"

-- create For Expr

createForExpr :: TypedId -> ForExpr -> Value
createForExpr ti (ForExpr (i1, e1) (i2, e2) e es) =
    VForExpr (Node (getNodeType exprNode1) (createId i1), exprNode1)
             (Node (getNodeType exprNode2) (createId i2), exprNode2)
             (createExprNode (createExpr ti e))
             (createExprsNode (createExprs ti es))
    where
        exprNode1 = createExprNode (createExpr ti e1)
        exprNode2 = createExprNode (createExpr ti e2)

createForExprNode :: Value -> Node
createForExprNode v@(VForExpr _ _ _ (Node t _)) = Node t v 
createForExprNode _ = Error "Typing of For Expr failed"

-- create If Expr

createIfExpr :: TypedId -> IfExpr -> Value
createIfExpr ti (IfExpr e es Nothing) =
    VIfExpr (createExprNode (createExpr ti e))
            (createExprsNode (createExprs ti es))
            createEmptyNode
createIfExpr ti (IfExpr e es1 (Just es2)) =
    VIfExpr (createExprNode (createExpr ti e))
            (createExprsNode (createExprs ti es1))
            (createExprsNode (createExprs ti es2))

createIfExprNode :: Value -> Node
createIfExprNode v@(VIfExpr _ (Node t _) _) = Node t v
createIfExprNode _ = Error "Typing of If Expr failed"

-- create While Expr

createWhileExpr :: TypedId -> WhileExpr -> Value
createWhileExpr ti (WhileExpr e es) =
    VWhileExpr (createExprNode (createExpr ti e))
               (createExprsNode (createExprs ti es))

createWhileExprNode :: Value -> Node
createWhileExprNode v@(VWhileExpr _ (Node t _)) = Node t v
createWhileExprNode _ = Error "Typing of While Expr failed"

-- create Exprs

createExprs :: TypedId -> Exprs -> Value
createExprs ti (EForExpr f) = VExprs [createForExprNode (createForExpr ti f)]
createExprs ti (EWhileExpr w) =
    VExprs [createWhileExprNode (createWhileExpr ti w)]
createExprs ti (EIfExpr i) = VExprs [createIfExprNode (createIfExpr ti i)]
createExprs ti (EExpr e) = VExprs (fmap (createExprNode . createExpr ti) e)

createExprsNode :: Value -> Node
createExprsNode v@(VExprs [Node t VWhileExpr {}]) = Node t v
createExprsNode v@(VExprs [Node t VForExpr {}]) = Node t v
createExprsNode v@(VExprs [Node t VIfExpr {}]) = Node t v
createExprsNode v@(VExprs (reverse -> ((Node t VExpr {}):_))) = Node t v
createExprsNode _ = Error "Typing of Exprs failed"

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

createPrototype :: TypedId -> Prototype -> (Value, TypedId)
createPrototype ti (Prototype i pa) =
    (VPrototype (Node (getNodeType protoArgsNode) (createId i))
               protoArgsNode,
    addArgsToTypedId ti args)
    where
        protoArgsNode@(Node _ (VPrototypeArgs args _)) =
            createPrototypeArgsNode (createPrototypeArgs pa)

createPrototypeNode :: Value -> Node
createPrototypeNode v@(VPrototype (Node t _) _) = Node t v
createPrototypeNode _ = Error "Typing of Prototype failed"

addArgsToTypedId :: TypedId -> [(Node, Node)] -> TypedId
addArgsToTypedId ti ((Node t (VIdentifier id), _):args) =
    addArgsToTypedId (Map.insert id t ti) args
addArgsToTypedId ti [] = ti
addArgsToTypedId ti _ = Map.empty

-- create Defs

createDefs :: TypedId -> Defs -> Value
createDefs ti (Defs p es) = VDefs prototypeNode
                                   (createExprsNode (createExprs newTi es))
    where
        (prototype, newTi) = createPrototype ti p
        prototypeNode = createPrototypeNode prototype

createDefsNode :: Value -> Node
createDefsNode v@(VDefs (Node t _) _) = Node t v
createDefsNode _ = Error "Typing of Defs failed"

-- create Kdefs

createKdefs :: TypedId -> Kdefs -> (Value, TypedId)
createKdefs ti (KDefs d) =
        (VKdefs defsNode, Map.insert id (getNodeType defsNode) ti)
    where
        defs@(VDefs (Node _ (VPrototype (Node _ (VIdentifier id)) _)) _) =
            createDefs ti d
        defsNode = createDefsNode defs
createKdefs ti (KExprs es) =
    (VKdefs (createExprsNode (createExprs ti es)), ti)

-- create Stmt

createStmt :: TypedId -> Stmt -> (Value, TypedId)
createStmt ti (k:ks) = case createStmt newTi ks of
        (VStmt list, endTi) -> (VStmt (Node TNone kdefs : list), endTi)
        (VError s, endTi) -> (VError s, endTi)
        (_, endTi) -> (VError "Typing of Stmt failed", endTi)
    where
        (kdefs, newTi) = createKdefs ti k
createStmt ti [] = (VStmt [], ti)

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

-- get Type

getNodeType :: Node -> Type 
getNodeType (Node t _) = t
getNodeType (Error s) = TError s

getExprType :: [Type] -> Type
getExprType ((TError s):_) = TError s
getExprType (TUndefine:_) = TError "Typing of Expr failed 1"
getExprType (TNone:_) = TError "Typing of Expr failed 2"
getExprType (TFunc list:_) = last list
getExprType (t:ts) = let restType = getExprType ts
                     in if t == restType || null ts
                        then t
                        else TError "Typing of Expr failed 3"
getExprType [] = TError "Typing of Expr failed 4"