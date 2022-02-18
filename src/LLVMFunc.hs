{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
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
litToLLVM _ = error (getErrorMessage "Literal")

-------- IDENTIFIER

getIdentifierName :: Node -> String 
getIdentifierName (Node _
                      (VUnary (Node _
                          (VPostfix (Node _
                              (VPrimary (Node _
                                  (VIdentifier s))))
                              (Node TNone VNothing)))
                          (Node TNone VNothing))) = s
getIdentifierName _ = error "Recuperation of Identifier string failed"

-------- PRIMARY

primaryToLLVM :: Node -> Codegen Operand
primaryToLLVM (Node _ (VPrimary (Node _ (VLiteral n)))) = litToLLVM n
primaryToLLVM (Node _ (VPrimary (Node _ (VIdentifier name)))) =
    loadIdentifier name
primaryToLLVM _ = error (getErrorMessage "Primary")

loadIdentifier :: String -> Codegen Operand
loadIdentifier name = do
    variables <- ask
    case variables Map.!? name of
        Just v -> do
                -- let var = LocalReference (Type.PointerType Type.double (AddrSpace 0)) (AST.Name $ fromString name)
                load' v
        Nothing -> decimalConstToLLVM (Node TInteger (VDecimalConst 42))

-------- POSTFIX

postfixToLLVM :: Node -> Codegen Operand
postfixToLLVM (Node _ (VPostfix (Node _ (VPrimary n)) (Node _ VNothing))) =
    primaryToLLVM n
postfixToLLVM _ = error (getErrorMessage "Postfix")

-------- EXPR

exprToLLVM :: Node -> Codegen Operand
exprToLLVM (Node _ (VExpr v ((op, v'):_))) = binopToLLVM op v v'
exprToLLVM (Node _ (VExpr v [])) = unaryToLLVM v
exprToLLVM _ = error (getErrorMessage "Expr")

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
        fct _ = error "Wrong Binop"

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

assignToLLVM :: Node -> Node -> Codegen Operand
assignToLLVM i u = mdo
    val <- unaryToLLVM u
    br assignBlock

    assignBlock <- block `named` "assignBlock"
    ptr <- alloca Type.i32 (Just (Const.int32 1)) 0
        `named` fromString (getIdentifierName i)
    store' ptr val
    let _ = Map.insert (getIdentifierName i) ptr
    load' ptr

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