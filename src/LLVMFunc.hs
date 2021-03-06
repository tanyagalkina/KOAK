{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BlockArguments #-}
module LLVMFunc where

-- IMPORT LLVM

-- import LLVM.CodeGenOpt
-- import LLVM.CodeModel
-- import LLVM.Context
-- import LLVM.Internal.OrcJIT.CompileLayer
-- import LLVM.Module
-- import LLVM.OrcJIT
-- import LLVM.Relocation
-- import LLVM.Target
-- import LLVM.PassManager
-- import LLVM.Pretty
-- import LLVM.Prelude (traverse)

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Float
import LLVM.AST.Constant
import qualified LLVM.AST.IntegerPredicate as Sicmp
import LLVM.AST.Type as Type
import LLVM.AST.Attribute
-- import LLVM.AST.Global
-- import LLVM.AST.AddrSpace
-- import LLVM.AST.FloatingPointPredicate hiding (False, True)
-- import LLVM.AST.Operand as Op

import LLVM.IRBuilder as IRB
import LLVM.IRBuilder.Constant as Const
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Instruction

-- IMPORT CONTROL.MONAD

import Control.Monad
import Control.Monad.Reader
-- import Control.Monad.Except
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans
-- import Control.Monad.Fix
-- import Control.Monad.List (ListT)

-- IMPORT DATA

import Data.String
-- import Data.IORef
-- import Data.Int
-- import Data.Word
-- import qualified Data.Ix as Type

-- import qualified Data.ByteString.Char8 as BS
-- import Data.ByteString.Builder.Prim (condB)

-- IMPORT SYSTEM..IO

-- import System.IO
-- import System.IO.Error

-- OTHER IMPORTS

import qualified Data.Map as Map
-- import Foreign.Ptr
import Prelude hiding (mod)
-- import Numeric
-- import Text.ParserCombinators.ReadP (endBy)

-- OUT IMPORTS

import Data (Value (..), Binop (..), Type (..), Node (..), Codegen, Unop (..), ArgsType(..), IsDeclaration, Boolean (..), AST)
import LLVM.AST.AddrSpace
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate(OGT, OEQ, ONE, OLT, OLE, OGE))


-------- HELPER

int :: Integer -> Operand
int = int32

toIntOpe :: Int -> Operand
toIntOpe x = int $ toInteger x

toFloatOpe :: Double -> Operand
toFloatOpe x = ConstantOperand (Float (LLVM.AST.Float.Double x))

typeToLLVMType ::  Data.Type -> LLVM.AST.Type
typeToLLVMType TInteger = Type.i32
typeToLLVMType TDouble = Type.double
typeToLLVMType _ = error "typeToLLVMType"

-------- GET PART OF NODE

nodeToVal :: Node -> Value
nodeToVal (Node _ v) = v
nodeToVal (Error s) = error s

nodeToType :: Node -> Data.Type
nodeToType (Node t _) = t
nodeToType (Error s) = error s

-------- BOOLEAN

boolToLLVM :: Node -> Codegen Operand
boolToLLVM (Node TBool (VBoolean Data.True)) = return $ bit 1
boolToLLVM (Node TBool (VBoolean Data.False)) = return $ bit 0
boolToLLVM n = error (getErrorMessage "Boolean" n)

-------- DECIMAL CONST

decimalConstToLLVM :: Node -> Codegen Operand
decimalConstToLLVM (Node TInteger (VDecimalConst d)) = return $ toIntOpe d
decimalConstToLLVM n = error (getErrorMessage "Decimal Const" n)

-------- DOUBLE CONST

doubleConstToLLVM :: Node -> Codegen Operand
doubleConstToLLVM (Node TDouble (VDoubleConst d)) = pure $ toFloatOpe d
doubleConstToLLVM n = error (getErrorMessage "Double Const" n)

-------- LITERAL

litToLLVM :: Node -> Codegen Operand
litToLLVM (Node TInteger (VLiteral n)) = decimalConstToLLVM n
litToLLVM (Node TDouble (VLiteral n)) = doubleConstToLLVM n
litToLLVM n = error (getErrorMessage "Literal" n)

-------- IDENTIFIER

loadIdentifierValue :: Node -> Codegen Operand
loadIdentifierValue (Node t (VIdentifier (name, _))) = do
    variables <- ask
    case variables Map.!? name of
        Just v -> pure v
        Nothing -> do
            let myType = typeToLLVMType t
            let var = LocalReference (Type.PointerType myType (AddrSpace 0)) (getName name)
            myLoad var
    where
        getName = AST.Name . fromString . stringToLLVMVarName
loadIdentifierValue n = error (getErrorMessage "Identifier" n)

updateIdentifier :: Node -> Operand -> Codegen Operand
updateIdentifier (Node t (VIdentifier (name, _))) val = do
    variables <- ask
    case variables Map.!? name of
        Just _ -> return val
        Nothing -> do
            let myType = typeToLLVMType t
            let var = LocalReference (Type.PointerType myType (AddrSpace 0)) (getName name)
            myStore var val
            return val
    where
        getName = AST.Name . fromString . stringToLLVMVarName
updateIdentifier n _ = error (getErrorMessage "Identifier" n)

stringToLLVMVarName :: String -> String
stringToLLVMVarName str = str ++ "_0"

getIdentifier :: Node -> (String, IsDeclaration)
getIdentifier (Node _ (VUnary (Node _ (VPostfix (Node _ (VPrimary
                  (Node _ (VIdentifier i)))) (Node TNone VNothing)))
                  (Node TNone VNothing))) = i
getIdentifier (Node _ (VPrimary (Node _ (VIdentifier i)))) = i
getIdentifier (Node _ (VPrototype (Node _ (VIdentifier i)) _)) = i
getIdentifier (Node _ (VIdentifier i)) = i
getIdentifier n = error ("Recuperation of Identifier string failed" ++ show n)

-------- PRIMARY

primaryToLLVM :: Node -> Codegen Operand
primaryToLLVM (Node _ (VPrimary n@(Node _ (VLiteral _)))) = litToLLVM n
primaryToLLVM (Node _ (VPrimary b@(Node _ (VBoolean _)))) = boolToLLVM b
primaryToLLVM (Node _ (VPrimary i@(Node _ (VIdentifier _)))) =
    loadIdentifierValue i
primaryToLLVM (Node _ (VPrimary n@(Node _ (VExprs _)))) = exprsToLLVM n
primaryToLLVM n = error (getErrorMessage "Primary" n)

-------- CALLEXPR

callExprToLLVM :: Node -> Codegen [(Operand, [ParameterAttribute])]
callExprToLLVM (Node _ (VCallExpr list)) = getAllArguments list
callExprToLLVM n = error (getErrorMessage "Call Expr" n)

getAllArguments :: [Node] -> Codegen [(Operand, [ParameterAttribute])]
getAllArguments (e@(Node _ (VExpr _ _)):rest) = do
    operand <- exprToLLVM e
    restOfArguments <- getAllArguments rest
    return ((operand, []) : restOfArguments)
getAllArguments [] = return []
getAllArguments _ = error (getErrorMessage "Call Expr" (Error ""))

-------- POSTFIX

postfixToLLVM :: Node -> Codegen Operand
postfixToLLVM (Node _ (VPostfix n (Node _ VNothing))) =
    primaryToLLVM n
postfixToLLVM (Node t (VPostfix n c)) = do
    let (retType, parsTypes) = getFunctionTypes t
    let ty    = FunctionType retType parsTypes Prelude.False
    let ptrTy = Type.PointerType ty (AddrSpace 0)
    let ref = GlobalReference ptrTy (fromString $ fst $ getIdentifier n)
    funcArgs <- callExprToLLVM c
    LLVM.IRBuilder.Instruction.call (ConstantOperand ref) funcArgs
postfixToLLVM n = error (getErrorMessage "Postfix" n)

getFunctionTypes :: Data.Type -> (AST.Type, [AST.Type])
getFunctionTypes t@(TFunc list) = case last list of
    TInteger -> (i32, getFunctionParsTypes t)
    TDouble -> (Type.double, getFunctionParsTypes t)
    TVoid -> (Type.void, getFunctionParsTypes t)
    _ -> (i32, getFunctionParsTypes t)
getFunctionTypes _ = error (getErrorMessage "Postfix" (Error ""))

getFunctionParsTypes :: Data.Type -> [AST.Type]
getFunctionParsTypes (TFunc [_]) = []
getFunctionParsTypes (TFunc (TInteger:t)) = i32 : getFunctionParsTypes (TFunc t)
getFunctionParsTypes (TFunc (TDouble:t)) = Type.double : getFunctionParsTypes (TFunc t)
getFunctionParsTypes (TFunc (TVoid:t)) = Type.void : getFunctionParsTypes (TFunc t)
getFunctionParsTypes _ = error (getErrorMessage "Postfix" (Error ""))

------- UNARY

unaryToLLVM :: Node -> Codegen Operand
unaryToLLVM (Node _ (VUnary (Node _ (VUnop Minus)) u)) = minusToLLVM u
unaryToLLVM (Node _ (VUnary (Node _ (VUnop Not)) u)) = notToLLVM u
unaryToLLVM (Node _ (VUnary p (Node _ VNothing))) = postfixToLLVM p
unaryToLLVM n = error (getErrorMessage "Unary" n)

------- UNOP

minusToLLVM :: Node -> Codegen Operand
minusToLLVM u@(Node t _) = case t of
    TInteger -> opToLLVM Data.Mul u (toIntOpe (-1))
    TDouble -> opToLLVM Data.Mul u (toFloatOpe (-1.0))
    _ -> error (getErrorMessage "Unary" u)
minusToLLVM u = error (getErrorMessage "Unary" u)

generateMinusOne :: Codegen Operand
generateMinusOne = unaryToLLVM (Node TInteger
                                (VUnary (Node TInteger
                                    (VPostfix (Node TInteger
                                        (VPrimary (Node TInteger
                                            (VLiteral (Node TInteger
                                                (VDecimalConst (-1)))))))
                                        (Node TNone VNothing)))
                                    (Node TNone VNothing)))

notToLLVM :: Node -> Codegen Operand
notToLLVM u@(Node t (VUnary _ _)) =  case t of
    TInteger -> eqToLLVM u (toIntOpe 0)
    TDouble  -> eqToLLVM u (toFloatOpe 0)
    TBool -> eqToLLVM u (bit 0)
    _ -> error (getErrorMessage "Not" (Error ""))
notToLLVM _ = error (getErrorMessage "Not" (Error ""))

-------- EXPR

exprToLLVM :: Node -> Codegen Operand
exprToLLVM (Node _ (VExpr u [])) = unaryToLLVM u
exprToLLVM (Node _ (VExpr u bus)) = binopToLLVM u bus
exprToLLVM n = error (getErrorMessage "Expr" n)

-------- BINOP

binopToLLVM :: Node -> [(Node, Node)] -> Codegen Operand
binopToLLVM u ((b, u'):bus) = do
    restRes <- binopToLLVM u' bus
    case nodeToVal b of
        (VBinop Data.Gt) -> gtToLLVM u restRes
        (VBinop Data.Lt) -> ltToLLVM u restRes
        (VBinop Data.Ge) -> geToLLVM u restRes
        (VBinop Data.Le) -> leToLLVM u restRes
        (VBinop Data.Eq) -> eqToLLVM u restRes
        (VBinop Data.Neq) -> neqToLLVM u restRes
        (VBinop Data.Assign) -> assignToLLVM u restRes
        (VBinop op') -> opToLLVM op' u restRes
        _ -> error (getErrorMessage "Binop" (Error ""))
binopToLLVM u [] = unaryToLLVM u

opToLLVM :: Binop -> Node -> Operand -> Codegen Operand
opToLLVM op u o = mdo
    br addBlock

    addBlock <- block `named` "opBlock"
    res <- fct op u o
    return res
    where
        fct Data.Add = addToLLVM
        fct Data.Sub = subToLLVm
        fct Data.Mul = mulToLLVM
        fct Data.Div = divToLLVM
        fct Data.Mod = modToLLVM
        fct _ = error (getErrorMessage "Binop" (Error ""))

getTypeAsSingleType :: Data.Type -> Data.Type
getTypeAsSingleType t = case t of
    TFunc list -> last list
    t' -> t'

modToLLVM :: Node -> Operand -> Codegen Operand
modToLLVM u@(Node t (VUnary _ _)) b = mdo
    a <- unaryToLLVM u
    case getTypeAsSingleType t of
        TInteger -> urem a b
        TDouble -> frem a b
        TBool -> urem a b
        _ -> error (getErrorMessage "Mod" (Error ""))
modToLLVM _ _ = error (getErrorMessage "Mod" (Error ""))

addToLLVM :: Node -> Operand -> Codegen Operand
addToLLVM u@(Node t (VUnary _ _)) b = mdo
    a <- unaryToLLVM u
    case getTypeAsSingleType t of
        TInteger -> add a b
        TDouble -> fadd a b
        TBool -> add a b
        _ -> error (getErrorMessage "Add" (Error ""))
addToLLVM _ _ = error (getErrorMessage "Add" (Error ""))

subToLLVm :: Node -> Operand -> Codegen Operand
subToLLVm u@(Node t (VUnary _ _)) b = mdo
    a <- unaryToLLVM u
    case getTypeAsSingleType t of
        TInteger -> sub a b
        TDouble -> fsub a b
        TBool -> sub a b
        _ -> error (getErrorMessage "Sub" (Error ""))
subToLLVm _ _ = error (getErrorMessage "Sub" (Error ""))

mulToLLVM :: Node -> Operand -> Codegen Operand
mulToLLVM u@(Node t (VUnary _ _)) b = mdo
    a <- unaryToLLVM u
    case getTypeAsSingleType t of
        TInteger -> mul a b
        TDouble -> fmul a b
        TBool -> mul a b
        _ -> error (getErrorMessage "Mul" (Error ""))
mulToLLVM _ _ = error (getErrorMessage "Mul" (Error ""))

divToLLVM :: Node -> Operand -> Codegen Operand
divToLLVM u@(Node t (VUnary _ _)) b = mdo
    a <- unaryToLLVM u
    case getTypeAsSingleType t of
        TInteger -> sdiv a b
        TDouble -> fdiv a b
        TBool -> sdiv a b
        _ -> error (getErrorMessage "Div" (Error ""))
divToLLVM _ _ = error (getErrorMessage "Div" (Error ""))

gtToLLVM :: Node -> Operand -> Codegen Operand
gtToLLVM u@(Node t (VUnary _ _)) b = unaryToLLVM u >>= \a ->
    case getTypeAsSingleType t of
        TInteger -> icmp Sicmp.SGT a b
        TDouble -> fcmp OGT a b
        TBool -> icmp Sicmp.SGT a b
        _ ->  error (getErrorMessage "Gt" (Error ""))
gtToLLVM n _ = error (getErrorMessage "Gt" n)


ltToLLVM :: Node -> Operand -> Codegen Operand
ltToLLVM u@(Node t (VUnary _ _)) b = unaryToLLVM u >>= \a ->
    case getTypeAsSingleType t of
        TInteger -> icmp Sicmp.SLT a b
        TDouble -> fcmp OLT  a b
        TBool -> icmp Sicmp.SLT a b
        _ ->  error (getErrorMessage "Lt" (Error ""))
ltToLLVM _ _ = error (getErrorMessage "Lt" (Error ""))


geToLLVM :: Node -> Operand -> Codegen Operand
geToLLVM u@(Node t (VUnary _ _)) b = unaryToLLVM u >>= \a ->
    case t of
        TInteger -> icmp Sicmp.SGE a b
        TDouble -> fcmp OGE a b
        TBool -> icmp Sicmp.SGE a b
        _ ->  error (getErrorMessage "Ge" (Error ""))
geToLLVM n _ = error (getErrorMessage "Ge" n)

leToLLVM :: Node -> Operand -> Codegen Operand
leToLLVM u@(Node t (VUnary _ _)) b = unaryToLLVM u >>= \a ->
    case t of
        TInteger -> icmp Sicmp.SLE a b
        TDouble -> fcmp OLE a b
        TBool -> icmp Sicmp.SLE a b
        _ ->  error (getErrorMessage "Lt" (Error ""))
leToLLVM _ _ = error (getErrorMessage "Lt" (Error ""))


eqToLLVM :: Node -> Operand -> Codegen Operand
eqToLLVM u@(Node t (VUnary _ _)) b = unaryToLLVM u >>= \a ->
    case getTypeAsSingleType t of
        TInteger -> icmp Sicmp.EQ a b
        TDouble -> fcmp OEQ a b-- >>= \x -> uitofp x Type.double
        TBool -> icmp Sicmp.EQ a b
        _ ->  error (getErrorMessage "Eq" (Error ""))
eqToLLVM _ _ = error (getErrorMessage "Eq" (Error ""))

neqToLLVM :: Node -> Operand -> Codegen Operand
neqToLLVM u@(Node t (VUnary _ _)) b = unaryToLLVM u >>= \a ->
    case getTypeAsSingleType t of
        TInteger -> icmp Sicmp.NE a b
        TDouble -> fcmp ONE  a b-- >>= \x -> uitofp x Type.double
        TBool -> icmp Sicmp.NE a b
        _ ->  error (getErrorMessage "Neq" (Error ""))
neqToLLVM _ _ = error (getErrorMessage "Neq" (Error ""))

assignToLLVM :: Node -> Operand -> Codegen Operand
assignToLLVM u@(Node t _) val = mdo
    let myType = typeToLLVMType t
    let (idString, isDec) = getIdentifier u
    br assignBlock

    assignBlock <- block `named` "assign.start"
    if isDec
    then do
        newVarPtr <- alloca myType (Just (Const.int32 1)) 0 `named` fromString idString
        myStore newVarPtr val
        withReaderT (Map.insert idString val) $ myLoad newVarPtr
    else do
        updateIdentifier (Node t (VIdentifier (idString, isDec))) val
assignToLLVM _ _ = error (getErrorMessage "Assign" (Error ""))

-------- WHILE

whileToLLVM :: Node -> Codegen Operand
whileToLLVM (Node _ (VWhileExpr e es)) = mdo
    let evalCond = (exprToLLVM e >>= icmp Sicmp.NE (bit 0))
    initCond <- evalCond `named` "while.initCond"
    condBr initCond loopB endBlock

    loopB <- block `named` "while.start"
    res <- exprsToLLVM es

    cond <- evalCond
    condBr cond loopB endBlock

    endBlock <- block `named` "while.end"
    return res
whileToLLVM n = error (getErrorMessage "While Expr" n)

-------- FOR

forToLLVM :: Node -> Codegen Operand
forToLLVM (Node _ (VForExpr (itName, itVal) (_, (Node _ (VExpr condVal _))) act instrs)) = mdo
    start <- currentBlock
    itVal' <- exprToLLVM itVal
    br begin

    begin <- block `named` "for.begin"
    loopVal <- phi [(itVal', start), (updatedVal, bodyEnd)] `named` "loopVal"
    res <- withReaderT (Map.insert (fst $ getIdentifier itName) loopVal) $ gtToLLVM condVal loopVal >>= \x -> icmp Sicmp.NE x (bit 0) `named` "test_block"
    condBr res bodyStart end

    bodyStart <- block `named` "for.body"
    res' <- withReaderT (Map.insert (fst $ getIdentifier itName) loopVal) $ exprsToLLVM instrs `named` "instr_block"

    updatedVal <-  withReaderT (Map.insert (fst $ getIdentifier itName) loopVal) $ exprToLLVM act `named` "act_block"

    bodyEnd <- currentBlock
    br begin

    end <- block `named` "for.end"
    return res'
forToLLVM n = error $ getErrorMessage "For Expr" n


-------- IF

ifToLLVM :: Node -> Codegen Operand
ifToLLVM (Node _ (VIfExpr cond thenInstr elseInstr)) = mdo
    condRes <- exprToLLVM cond
    condBr condRes thenBlock elseBlock

    thenBlock <- block `named` "if.then"
    thenRes <- exprsToLLVM thenInstr
    br end

    elseBlock <- block `named` "if.else"
    elseRes <- exprsToLLVM elseInstr
    br end

    end <- block `named` "if.end"
    res <- phi [(thenRes, thenBlock), (elseRes, elseBlock)] `named` "if.res"
    return res
ifToLLVM n = error (getErrorMessage "If Expr" n)

-------- EXPRS

exprsToLLVM :: Node -> Codegen Operand
exprsToLLVM (Node _ (VExprs [n@(Node _ (VWhileExpr _ _))])) = whileToLLVM n
exprsToLLVM (Node _ (VExprs [n@(Node _ (VForExpr {}))])) = forToLLVM n
exprsToLLVM (Node _ (VExprs [n@(Node _ (VIfExpr {}))])) = ifToLLVM n
exprsToLLVM (Node _ (VExprs [n@(Node _ (VExpr {}))])) = exprToLLVM n
exprsToLLVM (Node t (VExprs (n@(Node _ (VExpr {})):ns))) =
    exprToLLVM n >> exprsToLLVM (Node t (VExprs ns))
exprsToLLVM n = error (getErrorMessage "Exprs" n)

-------- ARGS TYPE

argsTypeToLLVM :: Node ->  AST.Type
argsTypeToLLVM (Node _ (VArgsType Data.Int)) = i32
argsTypeToLLVM (Node _ (VArgsType Data.Double)) = Type.double
argsTypeToLLVM (Node _ (VArgsType Void)) =  Type.void
argsTypeToLLVM n = error (getErrorMessage "Args Type" n)

-------- PROTOTYPE ARGS

prototypeArgsToLLVM :: Node -> ([(AST.Type, ParameterName)], AST.Type)
prototypeArgsToLLVM (Node _ (VPrototypeArgs iats at)) =
    (getAllParameters iats, argsTypeToLLVM at)
prototypeArgsToLLVM n = error (getErrorMessage "Prototype Args" n)

getAllParameters :: [(Node, Node)] -> [(AST.Type, ParameterName)]
getAllParameters (((Node _ (VIdentifier (s, _))), at): iats) =
    ((argsTypeToLLVM at, fromString s) : getAllParameters iats)
getAllParameters [] = []
getAllParameters _ = error (getErrorMessage "Prototype Args" (Error ""))

-------- PROTOTYPE

prototypeToLLVM :: Node -> ([(AST.Type, ParameterName)], AST.Type)
prototypeToLLVM (Node _ (VPrototype _ pa)) = prototypeArgsToLLVM pa
prototypeToLLVM n = error (getErrorMessage "Prototype" n)

-------- DEFS

defsToLLVM :: Node -> ModuleBuilder Operand
defsToLLVM (Node _ (VDefs p es)) = mdo
    let funcName = fst $ getIdentifier p
    let (pars, returnType) = prototypeToLLVM p
    fn <- LLVM.IRBuilder.Module.function (mkName funcName) pars
                                                           returnType
                                                           $ \funcArgs -> do
            let state = Map.fromList (zip (parametersToString pars) funcArgs)
            runReaderT (exprsToLLVM es) state >>= ret
    return fn
defsToLLVM n = error (getErrorMessage "Defs" n)

parametersToString :: [(AST.Type, ParameterName)] -> [String]
parametersToString list = (\(_, ParameterName s) -> init (tail (show s)))
                                <$> list

-------- KDEFS

kdefsToLLVM :: Node -> Operand -> Codegen Operand
kdefsToLLVM (Node _ (VKdefs es@(Node _ (VExprs _)))) _ = exprsToLLVM es
kdefsToLLVM (Node _ (VKdefs (Node _ (VComs _)))) prevRes = return prevRes
kdefsToLLVM n _ = error (getErrorMessage "Kdefs" n)

kdefsDefsToLLVM :: Node -> ModuleBuilder Operand
kdefsDefsToLLVM (Node _ (VKdefs d@(Node _ (VDefs _ _)))) = do
    defsToLLVM d
kdefsDefsToLLVM n = error (getErrorMessage "Kdefs" n)

-------- STMT

stmtToLLVM :: Node -> Operand -> ModuleBuilder ()
stmtToLLVM n@(Node _ (VStmt _)) prevRes = do
    let state = Map.empty
    _ <- stmtDefsToLLVM n
    _ <- LLVM.IRBuilder.Module.function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 $ \[_, _] -> do
        res <- runReaderT (stmtRestToLLVM n prevRes) state
        _ <- runReaderT (printResult n res) state
        ret (int32 0)
    pure ()
stmtToLLVM n _ = error (getErrorMessage "Stmt" n)

stmtDefsToLLVM :: Node -> ModuleBuilder Operand
stmtDefsToLLVM (Node t (VStmt list)) =
    case list of
        [kdefs@(Node _ (VKdefs (Node _ (VDefs _ _))))] -> kdefsDefsToLLVM kdefs
        (kdefs@(Node _ (VKdefs (Node _ (VDefs _ _)))):rest)
            -> kdefsDefsToLLVM kdefs >> stmtDefsToLLVM (Node t (VStmt rest))
        [] -> return (int32 0)
        (_:rest) -> stmtDefsToLLVM (Node t (VStmt rest))
stmtDefsToLLVM n = error (getErrorMessage "Stmt" n)

stmtRestToLLVM :: Node -> Operand -> Codegen Operand
stmtRestToLLVM (Node t (VStmt list)) prevRes = do
    case list of
        [(Node _ (VKdefs (Node _ (VDefs _ _))))]
            -> return (int32 0)
        ((Node _ (VKdefs (Node _ (VDefs _ _)))):rest)
            -> stmtRestToLLVM (Node t (VStmt rest)) prevRes
        [kdefs] -> kdefsToLLVM kdefs prevRes
        (kdefs:rest) -> do
             result <- kdefsToLLVM kdefs prevRes
             stmtRestToLLVM (Node t (VStmt rest)) result
        [] -> return (int32 0)
stmtRestToLLVM n _ = error (getErrorMessage "Stmt" n)

-------- PRINT RESULT

printResult :: AST -> Operand -> Codegen ()
printResult instr res = do
    case instr of
        (Node TInteger _) -> printFunc "printInt" i32
        (Node TBool _) -> printFunc "printBool" i32
        (Node TDouble _) -> printFunc "printDouble" Type.double
        _ -> do
            pure ()
    where
        printFunc name argType = do
            printFuncPtr <- extern name [argType] Type.void
            _ <- LLVM.IRBuilder.Instruction.call printFuncPtr [(res,[])]
            pure ()

-------- DATA MANAGEMENT

myLoad :: Operand -> Codegen Operand
myLoad adr = load adr 0

myStore :: Operand -> Operand -> Codegen ()
myStore adr = store adr 0

allocate :: LLVM.AST.Type -> Operand -> Codegen Operand
allocate ty val = do
    adr <- alloca ty (Just (IRB.int32 1)) 0
    myStore adr val
    pure adr

-------- ERROR

getErrorMessage :: String -> Node -> String
getErrorMessage s (Error _) = "Integration of " ++ s ++ " into LLVM module failed"
getErrorMessage s n = "Integration of " ++ s ++ " into LLVM module failed ~ " ++ show n