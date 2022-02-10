{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall  #-}
module CreateAST where

-- Import

import qualified Data.Map as Map

import Parser

import Data
import ParseCode

-- create AST / EDITABLE

createAST :: Parser AST
createAST = (\s -> let (stmt, ti) = createStmt Map.empty s
                   in  Node (TError (show ti)) stmt) <$> parseStmt

-- create Basic Nodes

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

createPrimary :: TypedId -> Primary -> (Value, TypedId)
createPrimary ti (PId i) =
    (VPrimary (createIdNode ti (createId i)), ti)
createPrimary ti (PLit l) = (VPrimary (createLiteralNode (createLiteral l)), ti)
createPrimary ti (PExprs es) =
    let (exprs, newTi) = createExprs ti es
    in (VPrimary (createExprsNode exprs), newTi)

createPrimaryNode :: Value -> Node
createPrimaryNode p@(VPrimary (Node t _)) = Node t p
createPrimaryNode _ = Error "Typing of Primary failed"

-- create Call Expr

createCallExpr :: TypedId -> CallExpr -> (Value, TypedId)
createCallExpr ti [] = (VCallExpr [createEmptyNode], ti)
createCallExpr ti callExpr =
    let (exprNodes, newTi) = createExprNodes ([], ti) callExpr
    in (VCallExpr exprNodes, newTi)

-- create Postfix

createPostfix :: TypedId -> Postfix -> (Value, TypedId)
createPostfix ti (Postfix p c) = case (c, primaryNode) of
    (Nothing, _) -> (VPostfix primaryNode createEmptyNode, newTi1)
    (Just c', Node t@(TFunc _) _) ->
        let (callExprNode, newTi2) = createCallExpr newTi1 c'
        in (VPostfix primaryNode (Node t callExprNode), newTi2)
    _ -> (VError "Typing of Postfix failed", ti)
    where
        (primary, newTi1) = createPrimary ti p
        primaryNode = createPrimaryNode primary

createPostfixNode :: Value -> Node
createPostfixNode v@(VPostfix (Node t _) _) = Node t v
createPostfixNode _ = Error "Typing of Postfix failed"

-- create Unary

createUnary :: TypedId -> Unary -> (Value, TypedId)
createUnary ti (Unop uno una) =
    let (unaryNode, newTi) = createUnary ti una
    in (VUnary (Node TNone (createUnop uno))
               (createUnaryNode unaryNode), newTi)
createUnary ti (UPostfix p) =
    let (postfixNode, newTi) = createPostfix ti p
    in (VUnary (createPostfixNode postfixNode) createEmptyNode, newTi)

createUnaryNode :: Value -> Node
createUnaryNode v@(VUnary (Node t (VPostfix _ _)) _) = Node t v
createUnaryNode v@(VUnary _ (Node t (VUnary _ _))) = Node t v
createUnaryNode _ = Error "Typing of Unary failed"

-- create Expr

createExpr :: TypedId -> Expr -> (Value, TypedId)
createExpr ti (Expr u bu) = (VExpr unaryNode (reverse reversedBuNode),
                             Map.union newTi1 newTi2)
    where
        (reversedBuNode, newTi1) =
            let (buNodes, newTi1') = createBinopUnaryNodes ([], ti) bu
            in applyAssignType newTi1' (reverse buNodes)
        (unaryNode, newTi2) =
            let (unary, newTi2') = createUnary ti u
            in case (reverse reversedBuNode, createUnaryNode unary) of
                    ([], uN) -> (uN, newTi2')
                    (x:_, uN) -> applyAssignTypeToFirst newTi2' x uN

createBinopUnaryNodes :: ([(Node, Node)], TypedId) -> [(Binop, Unary)]
                         -> ([(Node, Node)], TypedId)
createBinopUnaryNodes (list, ti) ((b, u):bus) =
    let (unary, newTi) = createUnary ti u
        binopNode = Node TNone (createBinop b)
    in createBinopUnaryNodes (list ++ [(binopNode, createUnaryNode unary)],
                              newTi)
                             bus
createBinopUnaryNodes res [] = res

applyAssignType :: TypedId -> [(Node, Node)] -> ([(Node, Node)], TypedId)
applyAssignType ti
                (bu@(Node _ (VBinop Assign), Node t _):bu'@(b, Node t' v):bus) =
    case t' of
        TNone -> let (newN, newTi1) = applyTypeToSubNode ti t (Node t v)
                     (newNs, newTi2) = applyAssignType ti ((b, Node t v) : bus)
                 in (bu : (b, newN) : tail newNs, Map.union newTi1 newTi2)
        _ -> let (newNs, newTi) = applyAssignType ti (bu' : bus)
             in (bu : bu' : tail newNs, newTi)
applyAssignType ti (bu:bus) = let (newNs, newTi) = applyAssignType ti bus
                              in (bu : newNs, newTi)
applyAssignType ti [] = ([], ti)

applyAssignTypeToFirst :: TypedId -> (Node, Node) -> Node -> (Node, TypedId)
applyAssignTypeToFirst ti (Node _ (VBinop Assign), Node t _) (Node t' v) =
    case t' of
        TNone -> applyTypeToSubNode ti t (Node t v)
        _ -> (Node t' v, ti)
applyAssignTypeToFirst ti _ u = (u, ti)

applyTypeToSubNode :: TypedId -> Type -> Node -> (Node, TypedId)
applyTypeToSubNode ti t node = case node of
        n@(Node _ (VUnary n1 n2)) -> applyTypeToTwoSubNode ti t n n1 n2
        n@(Node _ (VPostfix n1 n2)) -> applyTypeToTwoSubNode ti t n n1 n2
        (Node _ (VPrimary n)) -> let (newN, newTi) = applyTypeToSubNode ti t n
                                  in (Node t (VPrimary newN), newTi)
        (Node _ (VIdentifier i))
            -> (Node t (VIdentifier i), Map.insert i t ti)
        n -> (n, ti)

applyTypeToTwoSubNode :: TypedId -> Type -> Node -> Node -> Node
                         -> (Node, TypedId)
applyTypeToTwoSubNode ti t n n1 n2 = case n of
        (Node _ (VUnary _ _ )) -> (Node t (VUnary newN1 newN2), newTi)
        (Node _ (VPostfix _ _ )) -> (Node t (VPostfix newN1 newN2), newTi)
        _ -> (n, ti)
    where
        (newN1, newTi1) = applyTypeToSubNode ti t n1
        (newN2, newTi2) = applyTypeToSubNode ti t n2
        newTi = Map.union newTi1 newTi2

createExprNode :: Value -> Node
createExprNode v@(VExpr first list) =
    Node (getExprType (getNodeType first : fmap (getNodeType . snd) list)) v
createExprNode _ = Error "Typing of Expr failed"

-- create For Expr

createForExpr :: TypedId -> ForExpr -> (Value, TypedId)
createForExpr ti (ForExpr (i1, e1) (i2, e2) e es) =
        (VForExpr (Node (getNodeType exprNode1) (createId i1), exprNode1)
                (Node (getNodeType exprNode2) (createId i2), exprNode2)
                (createExprNode expr)
                (createExprsNode exprs), newTi3)
    where
        (expr1, newTi1) = createExpr ti e1
        exprNode1 = createExprNode expr1
        (expr2, newTi2) =
            createExpr (Map.insert i1 (getNodeType exprNode1) newTi1) e2
        exprNode2 = createExprNode expr2
        (expr, newTi) =
            createExpr (Map.insert i1 (getNodeType exprNode1) newTi2) e
        (exprs, newTi3) = createExprs newTi es

createForExprNode :: Value -> Node
createForExprNode v@(VForExpr _ _ _ (Node t _)) = Node t v 
createForExprNode _ = Error "Typing of For Expr failed"

-- create If Expr

createIfExpr :: TypedId -> IfExpr -> (Value, TypedId)
createIfExpr ti (IfExpr e es1 es2) = case es2 of
    Nothing -> (VIfExpr (createExprNode expr)
                       (createExprsNode exprs1)
                       createEmptyNode, newTi1)
    (Just es) -> let (exprs2, newTi2) = createExprs newTi1 es
                 in (VIfExpr (createExprNode expr)
                                    (createExprsNode exprs1)
                                    (createExprsNode exprs2), newTi2)
    where
        (expr, newTi) = createExpr ti e
        (exprs1, newTi1) = createExprs newTi es1

createIfExprNode :: Value -> Node
createIfExprNode v@(VIfExpr _ (Node t _) _) = Node t v
createIfExprNode _ = Error "Typing of If Expr failed"

-- create While Expr

createWhileExpr :: TypedId -> WhileExpr -> (Value, TypedId)
createWhileExpr ti (WhileExpr e es) =
    (VWhileExpr (createExprNode expr)
                (createExprsNode exprs), newTi2)
    where
        (expr, newTi1) = createExpr ti e
        (exprs, newTi2) = createExprs newTi1 es

createWhileExprNode :: Value -> Node
createWhileExprNode v@(VWhileExpr _ (Node t _)) = Node t v
createWhileExprNode _ = Error "Typing of While Expr failed"

-- create Exprs

createExprs :: TypedId -> Exprs -> (Value, TypedId)
createExprs ti (EForExpr f) = 
    let (forExpr, newTi) = createForExpr ti f
    in (VExprs [createForExprNode forExpr], newTi)
createExprs ti (EWhileExpr w) =
    let (whileExpr, newTi) = createWhileExpr ti w
    in (VExprs [createWhileExprNode whileExpr], newTi)
createExprs ti (EIfExpr i) =
    let (ifExpr, newTi) = createIfExpr ti i
    in (VExprs [createIfExprNode ifExpr], newTi)
createExprs ti (EExpr e) =
    let (exprNodes, newTi) = createExprNodes ([], ti) e
    in (VExprs exprNodes, newTi)

createExprNodes :: ([Node], TypedId) -> [Expr] -> ([Node], TypedId)
createExprNodes (list, ti) (expr:exprs) =
    let (newExpr, newTi) = createExpr ti expr
    in createExprNodes (list ++ [createExprNode newExpr], newTi) exprs
createExprNodes res [] = res

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
        (fmap (\(i, a') ->
                let argsTypeNode = createArgsTypeNode (createArgsType a')
                in (Node (getNodeType argsTypeNode) (createId i), argsTypeNode))
              ia)
        (createArgsTypeNode (createArgsType a))

createPrototypeArgsNode :: Value -> Node
createPrototypeArgsNode v@(VPrototypeArgs args ret) =
    case turnListOfTypeInFunc (fmap snd args ++ [ret]) of
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
addArgsToTypedId ti ((Node t (VIdentifier i), _):args) =
    addArgsToTypedId (Map.insert i t ti) args
addArgsToTypedId ti [] = ti
addArgsToTypedId _ _ = Map.empty

-- create Defs

createDefs :: TypedId -> Defs -> Value
createDefs ti (Defs p es) = VDefs prototypeNode
                                  (createExprsNode exprs)
    where
        (prototype, newTi1) = createPrototype ti p
        prototypeNode = createPrototypeNode prototype
        (exprs, _) = createExprs newTi1 es

createDefsNode :: Value -> Node
createDefsNode v@(VDefs (Node t _) _) = Node t v
createDefsNode _ = Error "Typing of Defs failed"

-- create Kdefs

createKdefs :: TypedId -> Kdefs -> (Value, TypedId)
createKdefs ti (KDefs d) =
        (VKdefs defsNode, Map.insert i (getNodeType defsNode) ti)
    where
        defs@(VDefs (Node _ (VPrototype (Node _ (VIdentifier i)) _)) _) =
            createDefs ti d
        defsNode = createDefsNode defs
createKdefs ti (KExprs es) = let (exprs, newTi) = createExprs ti es
                             in (VKdefs (createExprsNode exprs), newTi)
    

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
turnListOfTypeInFunc _ = TError "Typing of Prototype Args failed"

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