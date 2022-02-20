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

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Float
import LLVM.AST.Constant
import qualified LLVM.AST.IntegerPredicate as Sicmp
import LLVM.AST.Type as Type
-- import LLVM.AST.Global
-- import LLVM.AST.AddrSpace
-- import LLVM.AST.FloatingPointPredicate hiding (False, True)
-- import LLVM.AST.Operand as Op

import LLVM.IRBuilder as IRB
import LLVM.IRBuilder.Constant as Const
import LLVM.IRBuilder.Module
-- import LLVM.IRBuilder.Instruction


-- import LLVM.Prelude (traverse)

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
import MyLLVM (store', load')
import LLVM.AST.AddrSpace


-------- GET PART OF NODE

nodeToVal :: Node -> Value
nodeToVal (Node _ v) = v
nodeToVal (Error s) = error s

nodeToType :: Node -> Data.Type
nodeToType (Node t _) = t
nodeToType (Error s) = error s

-------- DECIMAL CONST

decimalConstToLLVM :: Node -> Codegen Operand
decimalConstToLLVM (Node TInteger (VDecimalConst d)) =
    return (int32 $ toInteger d)
decimalConstToLLVM n = error (getErrorMessage "Decimal Const" n)

-------- DOUBLE CONST

doubleConstToLLVM :: Node -> Codegen Operand
doubleConstToLLVM (Node TDouble (VDoubleConst d)) =
    pure $ ConstantOperand (Float (LLVM.AST.Float.Double d))
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
getIdentifier (Node _ (VPrototype (Node _ (VIdentifier s)) _)) = s
getIdentifier n = error ("Recuperation of Identifier string failed" ++ show n)

loadIdentifier :: String -> Codegen Operand
loadIdentifier name = do
    variables <- ask
    case variables Map.!? name of
        Just v -> pure v
        Nothing -> do
            let var = LocalReference (Type.PointerType Type.i32 (AddrSpace 0)) (AST.Name $ fromString (stringToLLVMVarName name))
            load var 0

stringToLLVMVarName :: String -> String
stringToLLVMVarName str = str ++ "_0"

-------- PRIMARY

primaryToLLVM :: Node -> Codegen Operand
primaryToLLVM (Node _ (VPrimary n@(Node _ (VLiteral _)))) = litToLLVM n
primaryToLLVM (Node _ (VPrimary (Node _ (VIdentifier name)))) =
    loadIdentifier name
primaryToLLVM (Node _ (VPrimary n@(Node _ (VExprs _)))) = exprsToLLVM n
primaryToLLVM n = error (getErrorMessage "Primary" n)

-------- POSTFIX

postfixToLLVM :: Node -> Codegen Operand
postfixToLLVM (Node _ (VPostfix n (Node _ VNothing))) =
    primaryToLLVM n
postfixToLLVM n = error (getErrorMessage "Postfix" n)

------- UNARY

unaryToLLVM :: Node -> Codegen Operand
unaryToLLVM (Node _ (VUnary (Node _ (VUnop Minus)) u)) =
    minusToLLVM u
unaryToLLVM (Node _ (VUnary p (Node _ VNothing))) = postfixToLLVM p
unaryToLLVM _ = error "Unknown type"

-------- EXPR

exprToLLVM :: Node -> Codegen Operand
exprToLLVM (Node _ (VExpr v ((op, v'):_))) = binopToLLVM op v v'
exprToLLVM (Node _ (VExpr v [])) = unaryToLLVM v
exprToLLVM n = error (getErrorMessage "Expr" n)

-------- BINOP

binopToLLVM :: Node -> Node -> Node -> Codegen Operand
binopToLLVM b u u' = case nodeToVal b of
  (VBinop Data.Gt) -> gtToLLVM u u'
  (VBinop Data.Lt) -> ltToLLVM u u'
  (VBinop Data.Eq) -> eqToLLVM u u'
  (VBinop Data.Neq) -> neqToLLVM u u'
  (VBinop Data.Assign) -> assignToLLVM u u'
  (VBinop op') -> opToLLVM op' u u'
  _ -> error (getErrorMessage "Binop" (Error ""))

opToLLVM :: Binop -> Node -> Node -> Codegen Operand
opToLLVM op u u' = mdo
    br addBlock

    addBlock <- block `named` "opBlock"
    -- myPuts <- extern "puts" [ptr i8] i32
    -- plouf <- LLVM.IRBuilder.Instruction.call myPuts []
    res <- fct op u u'
    -- a2 <- valueToLLVM (VDecimalConst 0)
    -- b2 <- valueToLLVM (VDecimalConst 0)
    -- test <- add a2 b2
    return res
    where
        fct Data.Add = addToLLVM
        fct Data.Sub = subToLLVm
        fct Data.Mul = mulToLLVM
        fct Data.Div = divToLLVM
        fct _ = error (getErrorMessage "Binop" (Error ""))

addToLLVM :: Node -> Node -> Codegen Operand
addToLLVM u@(Node t (VUnary _ _)) u' = mdo
    a <- unaryToLLVM u
    b <- unaryToLLVM u'
    case t of
        TInteger -> add a b
        TDouble -> fadd a b
        _ -> error (getErrorMessage "Add" (Error ""))
addToLLVM _ _ = error (getErrorMessage "Add" (Error ""))

subToLLVm :: Node -> Node -> Codegen Operand
subToLLVm u@(Node t (VUnary _ _)) u' = mdo
    a <- unaryToLLVM u
    b <- unaryToLLVM u'
    case t of
        TInteger -> sub a b
        TDouble -> fsub a b
        _ -> error (getErrorMessage "Sub" (Error ""))
subToLLVm _ _ = error (getErrorMessage "Sub" (Error ""))

mulToLLVM :: Node -> Node -> Codegen Operand
mulToLLVM u@(Node t (VUnary _ _)) u' = mdo
    a <- unaryToLLVM u
    b <- unaryToLLVM u'
    case t of
        TInteger -> mul a b
        TDouble -> fmul a b
        _ -> error (getErrorMessage "Mul" (Error ""))
mulToLLVM _ _ = error (getErrorMessage "Mul" (Error ""))

divToLLVM :: Node -> Node -> Codegen Operand
divToLLVM u@(Node t (VUnary _ _)) u' = mdo
    a <- unaryToLLVM u
    b <- unaryToLLVM u'
    case t of
        TInteger -> sdiv a b
        TDouble -> fdiv a b
        _ -> error (getErrorMessage "Div" (Error ""))
divToLLVM _ _ = error (getErrorMessage "Div" (Error ""))

gtToLLVM :: Node -> Node  -> Codegen Operand
gtToLLVM u u' = mdo
    a <- unaryToLLVM u
    b <- unaryToLLVM u'
    br subBlock

    subBlock <- block `named` "gtBlock"
    res <- icmp Sicmp.SGT a b
    return res

ltToLLVM :: Node -> Node  -> Codegen Operand
ltToLLVM u u' = mdo
    a <- unaryToLLVM u
    b <- unaryToLLVM u'
    br subBlock

    subBlock <- block `named` "ltBlock"
    res <- icmp Sicmp.SLT a b
    return res

eqToLLVM :: Node -> Node  -> Codegen Operand
eqToLLVM u u' = mdo
    a <- unaryToLLVM u
    b <- unaryToLLVM u'
    br subBlock

    subBlock <- block `named` "eqBlock"
    res <- icmp Sicmp.EQ  a b
    return res

neqToLLVM :: Node -> Node  -> Codegen Operand
neqToLLVM u u' = mdo
    a <- unaryToLLVM u
    b <- unaryToLLVM u'
    br subBlock

    subBlock <- block `named` "neqBlock"
    res <- icmp Sicmp.NE a b
    return res

assignToLLVM :: Node -> Node  -> Codegen Operand
assignToLLVM i u = mdo
    val <- unaryToLLVM u
    br assignBlock

    assignBlock <- block `named` "assign.start"
    ptr <- alloca Type.i32 (Just (Const.int32 1)) 0 `named` fromString (getIdentifier i)
    store' ptr val
    -- res <- addBind varName ptr
    -- varia <- ask
    -- Map.insert varName ptr varia
    withReaderT (Map.insert (getIdentifier i) ptr) $ load' ptr
    -- error $ "assign: " ++ show varia
    -- return res

-- assignToLLVM (VIdentifier varName) varValue = do
--     val <- valueToLLVM varValue
--     ptr' <- alloca Type.i32 (Just $ ConstantOperand (Int 128 4)) 8 `named` fromString varName
--     let tmp = Map.insert varName ptr
--     store ptr' 8 val
--     load ptr' 8

-- checkBind :: Codegen Operand -> Codegen Operand
-- checkBind a = do
--     a' <- ask
--     error $ "check: " ++ show a'

-- addBind :: String -> Operand -> Codegen Operand
-- addBind name ptr' = do
--     binds <- ask
--     -- pure $ Map.fromList [("lol", int32 42)]
--     -- res <- withReaderT (Map.fromList [("lol", int32 42)]) $ load' ptr'
--     local (Map.insert name ptr') $ load' ptr'
--     -- checkBind $ withReaderT (Map.insert name ptr') (load' ptr')
--     -- return $( Map.insert name ptr') binds
--     load' ptr'

------- UNOP

minusToLLVM :: Node -> Codegen Operand
minusToLLVM u = mdo
    res <- opToLLVM Data.Mul u generateMinusOne
    return res

generateMinusOne :: Node
generateMinusOne =
    Node TInteger
        (VUnary (Node TInteger
            (VPostfix (Node TInteger
                (VPrimary (Node TInteger
                    (VLiteral (Node TInteger
                        (VDecimalConst (-1)))))))
                (Node TNone VNothing)))
            (Node TNone VNothing))

-------- WHILE

whileToLLVM :: Node -> Codegen Operand
whileToLLVM (Node _ (VWhileExpr e es)) = mdo
    let evalCond = (exprsToLLVM e >>= icmp Sicmp.NE (int32 0))
    initCond <- evalCond `named` "while.initCond"
    condBr initCond loopB endBlock

    loopB <- block `named` "while.start"
    _ <- exprsToLLVM es

    cond <- evalCond
    condBr cond loopB endBlock

    endBlock <- block `named` "while.end"
    return $ int32 0
whileToLLVM n = error (getErrorMessage "While Expr" n)


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


ifToLLVM :: Node -> Node -> Node -> Codegen Operand
ifToLLVM cond thenInstr elseInstr = mdo
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

-------- EXPRS

exprsToLLVM :: Node -> Codegen Operand
exprsToLLVM (Node _ (VExprs [n@(Node _ (VWhileExpr _ _))])) = whileToLLVM n
exprsToLLVM (Node _ (VExprs [n@(Node _ (VForExpr {}))])) = exprToLLVM n
exprsToLLVM (Node _ (VExprs [n@(Node _ (VIfExpr {}))])) = exprToLLVM n
exprsToLLVM (Node _ (VExprs [n@(Node _ (VExpr {}))])) = exprToLLVM n
exprsToLLVM (Node t (VExprs (n@(Node _ (VExpr {})):ns))) = exprToLLVM n >>
                                       exprsToLLVM (Node t (VExprs ns))
exprsToLLVM n = error (getErrorMessage "Exprs" n)

-------- ARGS TYPE

argsTypeToLLVM :: Node ->  AST.Type
argsTypeToLLVM (Node _ (VArgsType Data.Int)) = i32
argsTypeToLLVM (Node _ (VArgsType Data.Double)) = Type.double
argsTypeToLLVM (Node _ (VArgsType Void)) =  Type.void
argsTypeToLLVM n = error (getErrorMessage "Args Type" n)

-------- PROTOTYPE ARGS

prototypeArgsToLLVM :: Node -> ([(AST.Type, ParameterName)], AST.Type)
prototypeArgsToLLVM (Node _ (VPrototypeArgs iats at)) = (getAllParameters iats, argsTypeToLLVM at)
prototypeArgsToLLVM n = error (getErrorMessage "Prototype Args" n)

getAllParameters :: [(Node, Node)] -> [(AST.Type, ParameterName)]
getAllParameters (((Node _ (VIdentifier s)), at): iats) = ((argsTypeToLLVM at, fromString s) : getAllParameters iats)
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
    let (parameters, returnType) = prototypeArgsToLLVM p
    fn <- LLVM.IRBuilder.Module.function (mkName funcName) parameters returnType $ \args -> do
            let params = Map.fromList (zip (parametersToString parameters) args)
            runReaderT (exprsToLLVM es) params >>= ret
    return fn
defsToLLVM n = error (getErrorMessage "Defs" n)

parametersToString :: [(AST.Type, ParameterName)] -> [String]
parametersToString lst = aux <$> lst
    where aux x = show $  snd x

-- _ <- LLVM.IRBuilder.Module.function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 $ \[_, _] -> do
--         res <- runReaderT (compileInstrs instr) state
--         ret res

-------- ERROR

getErrorMessage :: String -> Node -> String
getErrorMessage s (Error _) = "Integration of " ++ s ++ " into LLVM module failed"
getErrorMessage s n = "Integration of " ++ s ++ " into LLVM module failed ~ " ++ show n