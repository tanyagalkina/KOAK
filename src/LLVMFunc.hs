{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
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
-- import LLVM.IRBuilder.Instruction
-- import LLVM.IRBuilder.Module


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

import Data (Value (..), Binop (..), Type (..), Node (..), Codegen, Unop (..))
import MyLLVM (store', load')
import qualified LLVM.AST as AST
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
decimalConstToLLVM _ = error (getErrorMessage "Decimal Const")

doubleConstToLLVM :: Node -> Codegen Operand
doubleConstToLLVM (Node TInteger (VDoubleConst d)) =
    pure $ ConstantOperand (Float (LLVM.AST.Float.Double d))
doubleConstToLLVM _ = error (getErrorMessage "Double Const")

-------- LITERAL

litToLLVM :: Node -> Codegen Operand
litToLLVM (Node TInteger (VLiteral n)) = decimalConstToLLVM n
litToLLVM (Node TDouble (VLiteral n)) = doubleConstToLLVM n
litToLLVM n = error ((getErrorMessage "Literal") ++ show n)

-------- IDENTIFIER

getIdentifierName :: Node -> String 
getIdentifierName (Node _
                      (VUnary (Node _
                          (VPostfix (Node _
                              (VPrimary (Node _
                                  (VIdentifier s))))
                              (Node TNone VNothing)))
                          (Node TNone VNothing))) = s
getIdentifierName n = error ("Recuperation of Identifier string failed" ++ show n)

-------- PRIMARY

primaryToLLVM :: Node -> Codegen Operand
primaryToLLVM (Node _ (VPrimary (Node _ (VLiteral n)))) = litToLLVM n
primaryToLLVM (Node _ (VPrimary (Node _ (VIdentifier name)))) =
    loadIdentifier name
primaryToLLVM n = error ((getErrorMessage "Primary") ++ show n)

loadIdentifier :: String -> Codegen Operand
loadIdentifier name = do
    variables <- ask
    case variables Map.!? name of
        Just v -> do
            -- let var = LocalReference (Type.PointerType Type.double (AddrSpace 0)) (AST.Name $ fromString name)
            load' v
        -- Nothing -> valueToLLVM (VDecimalConst 42)
        Nothing -> do -- error $ "LoadIndentifier" ++ show variables
            let var = LocalReference (Type.PointerType Type.i32 (AddrSpace 0)) (AST.Name $ fromString "lolilol_0")
            load var 0

-------- POSTFIX

postfixToLLVM :: Node -> Codegen Operand
postfixToLLVM (Node _ (VPostfix (Node _ (VPrimary n)) (Node _ VNothing))) =
    primaryToLLVM n
postfixToLLVM n = error ((getErrorMessage "Postfix") ++ show n)

-------- EXPR

exprsToLLVM :: [Node] -> Codegen Operand
exprsToLLVM [] = return $ int32 0
exprsToLLVM [x] = exprToLLVM x
exprsToLLVM (x:xs) = exprToLLVM x  >> exprsToLLVM xs


exprToLLVM :: Node -> Codegen Operand
exprToLLVM (Node _ (VExpr v ((op, v'):_))) = binopToLLVM op v v'
exprToLLVM (Node _ (VExpr v [])) = unaryToLLVM v
exprToLLVM n = error ((getErrorMessage "Expr") ++ show n)

-------- BINOP

{-
    Operator -> Value 1 -> Value 2 -> result
-}
binopToLLVM :: Node -> Node -> Node -> Codegen Operand
binopToLLVM b u u' = case nodeToVal b of
  (VBinop Data.Gt) -> gtToLLVM u u'
  (VBinop Data.Lt) -> ltToLLVM u u'
  (VBinop Data.Eq) -> eqToLLVM u u'
  (VBinop Data.Neq) -> neqToLLVM u u'
  (VBinop Data.Assign) -> assignToLLVM u u'
  (VBinop op') -> opToLLVM op' u u'
  _ -> error (getErrorMessage "Binop")

opToLLVM :: Binop -> Node -> Node -> Codegen Operand
opToLLVM op u u' = mdo
    a <- unaryToLLVM u
    b <- unaryToLLVM u'
    br addBlock

    addBlock <- block `named` "opBlock"
    -- myPuts <- extern "puts" [ptr i8] i32
    -- plouf <- LLVM.IRBuilder.Instruction.call myPuts []
    res <- fct op a b
    -- a2 <- valueToLLVM (VDecimalConst 0)
    -- b2 <- valueToLLVM (VDecimalConst 0)
    -- test <- add a2 b2
    return res
    where
        fct Data.Add = add
        fct Data.Sub = sub
        fct Data.Mul = mul
        fct Data.Div = sdiv
        fct _ = error (getErrorMessage "Binop")

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
    ptr <- alloca Type.i32 (Just (Const.int32 1)) 0 `named` fromString "lolilol"
    store' ptr val
    -- res <- addBind varName ptr
    -- varia <- ask
    -- Map.insert varName ptr varia
    withReaderT (Map.insert (getIdentifierName i) ptr) $ load' ptr
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

------- UNARY

unaryToLLVM :: Node -> Codegen Operand
unaryToLLVM (Node _ (VUnary (Node _ (VUnop Minus)) u)) =
    minusToLLVM u
unaryToLLVM (Node _ (VUnary p (Node _ VNothing))) = postfixToLLVM p
unaryToLLVM _ = error "Unknown type"

------- UNOP

minusToLLVM :: Node -> Codegen Operand
minusToLLVM u = mdo
    res <- opToLLVM Data.Mul u generateMinusOne
    return res

generateMinusOne :: Node
generateMinusOne =
    Node TUndefine
        (VUnary (Node TUndefine
            (VPostfix (Node TUndefine
                (VPrimary (Node TUndefine
                    (VLiteral (Node TInteger
                        (VDecimalConst (-1)))))))
                (Node TNone VNothing)))
            (Node TNone VNothing))

-------- WHILE

-- whileToLLVM :: Value -> Codegen Operand
-- whileToLLVM v = undefined

-------- ERROR

getErrorMessage :: String -> String 
getErrorMessage s = "Integration of " ++ s ++ " into LLVM module failed"