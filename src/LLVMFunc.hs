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

import Data (Value (..), Binop (..), Type (..), Node (..), Codegen, Unop (..), ArgsType(..))
import LLVM.AST.AddrSpace
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate(OGT, OEQ, ONE, OLT))


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

getIdentifier :: Node -> String
getIdentifier (Node _ (VUnary (Node _ (VPostfix (Node _ (VPrimary
                  (Node _ (VIdentifier s)))) (Node TNone VNothing)))
                  (Node TNone VNothing))) = s
getIdentifier (Node _ (VPrimary (Node _ (VIdentifier s)))) = s
getIdentifier (Node _ (VPrototype (Node _ (VIdentifier s)) _)) = s
getIdentifier n = error ("Recuperation of Identifier string failed" ++ show n)

loadIdentifier :: Node -> Codegen Operand
loadIdentifier (Node t (VIdentifier name)) = do
    variables <- ask
    case variables Map.!? name of
        Just v -> pure v
        Nothing -> do
            let myType = typeToLLVMType t
            let var = LocalReference (Type.PointerType myType (AddrSpace 0)) (getName name)
            load var 0
    where
        getName = AST.Name . fromString . stringToLLVMVarName
loadIdentifier n = error (getErrorMessage "Identifier" n)

stringToLLVMVarName :: String -> String
stringToLLVMVarName str = str ++ "_0"

-------- PRIMARY

primaryToLLVM :: Node -> Codegen Operand
primaryToLLVM (Node _ (VPrimary n@(Node _ (VLiteral _)))) = litToLLVM n
primaryToLLVM (Node _ (VPrimary i@(Node _ (VIdentifier _)))) =
    loadIdentifier i
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
postfixToLLVM (Node _ (VPostfix n c)) = do
    let ptrTy = Type.PointerType i32 (AddrSpace 0)
    let ref = GlobalReference ptrTy (fromString $ getIdentifier n)
    funcArgs <- callExprToLLVM c
    LLVM.IRBuilder.Instruction.call (ConstantOperand ref) funcArgs
postfixToLLVM n = error (getErrorMessage "Postfix" n)

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
        fct _ = error (getErrorMessage "Binop" (Error ""))

addToLLVM :: Node -> Operand -> Codegen Operand
addToLLVM u@(Node t (VUnary _ _)) b = mdo
    a <- unaryToLLVM u
    case t of
        TInteger -> add a b
        TDouble -> fadd a b
        _ -> error (getErrorMessage "Add" (Error ""))
addToLLVM _ _ = error (getErrorMessage "Add" (Error ""))

subToLLVm :: Node -> Operand -> Codegen Operand
subToLLVm u@(Node t (VUnary _ _)) b = mdo
    a <- unaryToLLVM u
    case t of
        TInteger -> sub a b
        TDouble -> fsub a b
        _ -> error (getErrorMessage "Sub" (Error ""))
subToLLVm _ _ = error (getErrorMessage "Sub" (Error ""))

mulToLLVM :: Node -> Operand -> Codegen Operand
mulToLLVM u@(Node t (VUnary _ _)) b = mdo
    a <- unaryToLLVM u
    case t of
        TInteger -> mul a b
        TDouble -> fmul a b
        _ -> error (getErrorMessage "Mul" (Error ""))
mulToLLVM _ _ = error (getErrorMessage "Mul" (Error ""))

divToLLVM :: Node -> Operand -> Codegen Operand
divToLLVM u@(Node t (VUnary _ _)) b = mdo
    a <- unaryToLLVM u
    case t of
        TInteger -> sdiv a b
        TDouble -> fdiv a b
        _ -> error (getErrorMessage "Div" (Error ""))
divToLLVM _ _ = error (getErrorMessage "Div" (Error ""))

gtToLLVM :: Node -> Operand -> Codegen Operand
gtToLLVM u@(Node t (VUnary _ _)) b = unaryToLLVM u >>= \a ->
    case t of
        TInteger -> icmp Sicmp.SGT a b
        TDouble -> fcmp OGT a b >>= \x -> uitofp x Type.double
        _ ->  error (getErrorMessage "Gt" (Error ""))
gtToLLVM _ _ = error (getErrorMessage "Gt" (Error ""))


ltToLLVM :: Node -> Operand -> Codegen Operand
ltToLLVM u@(Node t (VUnary _ _)) b = unaryToLLVM u >>= \a ->
    case t of
        TInteger -> icmp Sicmp.SLT a b
        TDouble -> fcmp OLT  a b >>= \x -> uitofp x Type.double
        _ ->  error (getErrorMessage "Lt" (Error ""))
ltToLLVM _ _ = error (getErrorMessage "Lt" (Error ""))

eqToLLVM :: Node -> Operand -> Codegen Operand
eqToLLVM u@(Node t (VUnary _ _)) b = unaryToLLVM u >>= \a ->
    case t of
        TInteger -> icmp Sicmp.EQ a b
        TDouble -> fcmp OEQ a b >>= \x -> uitofp x Type.double
        _ ->  error (getErrorMessage "Eq" (Error ""))
eqToLLVM _ _ = error (getErrorMessage "Eq" (Error ""))

neqToLLVM :: Node -> Operand -> Codegen Operand
neqToLLVM u@(Node t (VUnary _ _)) b = unaryToLLVM u >>= \a ->
    case t of
        TInteger -> icmp Sicmp.NE a b
        TDouble -> fcmp ONE  a b >>= \x -> uitofp x Type.double
        _ ->  error (getErrorMessage "Neq" (Error ""))
neqToLLVM _ _ = error (getErrorMessage "Neq" (Error ""))

assignToLLVM :: Node -> Operand -> Codegen Operand
assignToLLVM i@(Node t _) val = mdo
    let myType = typeToLLVMType t
    br assignBlock

    assignBlock <- block `named` "assign.start"
    ptr <- alloca myType (Just (Const.int32 1)) 0 `named` fromString (getIdentifier i)
    myStore ptr val
    withReaderT (Map.insert (getIdentifier i) ptr) $ myLoad ptr
assignToLLVM _ _ = error "Assign unknown type"

-------- WHILE

whileToLLVM :: Node -> Codegen Operand
whileToLLVM (Node _ (VWhileExpr e es)) = mdo
    let evalCond = (exprToLLVM e >>= icmp Sicmp.NE (int32 0))
    initCond <- evalCond `named` "while.initCond"
    condBr initCond loopB endBlock

    loopB <- block `named` "while.start"
    _ <- exprsToLLVM es

    cond <- evalCond
    condBr cond loopB endBlock

    endBlock <- block `named` "while.end"
    return $ int32 0
whileToLLVM n = error (getErrorMessage "While Expr" n)

-------- FOR

forToLLVM :: Node -> Codegen Operand
forToLLVM = undefined

-- forToLLVM :: (Node, Node) -> (Node, Node) -> Node -> Node -> Codegen Operand
-- forToLLVM (itName, itVal) (condName, condVal) act instrs = mdo
--     itVal' <- exprToLLVM itVal
--     start <- currentBlock
--     br begin

--     begin <- block `named` "for.begin"
--     loopVal <- phi [(itVal', start), (updatedVal, bodyEnd)]
--     res <- ltToLLVM loopVal  condVal
--     condBr res bodyStart end

--     bodyStart <- block `named` "for.body"
--     _ <- withReaderT (Map.insert (getIdentifierName itName) loopVal) $ exprsToLLVM instrs

--     updatedVal <-  withReaderT (Map.insert (getIdentifierName itName) loopVal) $ exprToLLVM act
--     bodyEnd <- currentBlock
--     br begin

--     end <- block `named` "for.end"
--     return $ int32 0

-------- IF

ifToLLVM :: Node -> Codegen Operand
ifToLLVM (Node _ (VIfExpr cond thenInstr elseInstr)) = mdo
    condRes <- exprToLLVM cond
    condBr condRes thenBlock elseBlock

    thenBlock <- block `named` "if.then"
    _ <- exprsToLLVM thenInstr
    br end

    elseBlock <- block `named` "if.else"
    _ <- exprsToLLVM elseInstr
    br end

    end <- block `named` "if.end"
    return $ int32 0
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
getAllParameters (((Node _ (VIdentifier s)), at): iats) =
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
    let funcName = getIdentifier p
    let (pars, returnType) = prototypeArgsToLLVM p
    fn <- LLVM.IRBuilder.Module.function (mkName funcName) pars
                                                           returnType
                                                           $ \funcArgs -> do
            let state = Map.fromList (zip (parametersToString pars) funcArgs)
            runReaderT (exprsToLLVM es) state >>= ret
    return fn
defsToLLVM n = error (getErrorMessage "Defs" n)

parametersToString :: [(AST.Type, ParameterName)] -> [String]
parametersToString list = (show . snd) <$> list

-------- KDEFS

kdefsToLLVM :: Node -> Codegen Operand
kdefsToLLVM (Node _ (VKdefs es@(Node _ (VExprs _)))) = exprsToLLVM es
kdefsToLLVM (Node _ (VKdefs (Node _ (VDefs _ _)))) = do
    -- plouf <- defsToLLVM d
    return $ int32 0
kdefsToLLVM n = error (getErrorMessage "Kdefs" n)

-------- STMT

stmtToLLVM :: Node -> Codegen Operand
stmtToLLVM (Node t (VStmt (kdefs@(Node _ (VKdefs _)) : rest))) =
    case rest of
        [] -> kdefsToLLVM kdefs
        _ -> kdefsToLLVM kdefs >> stmtToLLVM (Node t (VStmt rest))
stmtToLLVM n = error (getErrorMessage "Stmt" n)

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