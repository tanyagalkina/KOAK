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

import Data (Value (..), Binop (..), Type (..), AST, Node (..), Codegen, Unop (..))
import MyLLVM (store', load')

-------- GET PART OF NODE

nodeToVal :: AST -> Value
nodeToVal (Node _ v) = v

nodeToType :: AST -> Data.Type
nodeToType (Node t _) = t

--------- TMP

nodeToValTmp :: Node -> Value
nodeToValTmp (Node _ (VUnary (Node _ (VPostfix (Node _ (VPrimary (Node _ (VLiteral (Node _ val))))) _)) _)) = val
nodeToValTmp _ = error "error: node to value"

-------- LITERAL

litToLLVM :: Node -> Codegen Operand
litToLLVM (Node TInteger (VDecimalConst v)) = return (int32 $ toInteger v)
litToLLVM (Node TDouble (VDoubleConst v)) =
    pure $ ConstantOperand (Float (LLVM.AST.Float.Double v))
litToLLVM _ = error "Unknown type"

valueToLLVM :: Value -> Codegen Operand
valueToLLVM (VDecimalConst v) = return (int32 $ toInteger v)
valueToLLVM (VDoubleConst v) = pure $ ConstantOperand (Float (LLVM.AST.Float.Double v))
valueToLLVM _ = error "Unknown type"

-------- PRIMARY

primaryToLLVM :: Node -> Codegen Operand
primaryToLLVM (Node _ (VLiteral v)) = litToLLVM v
primaryToLLVM (Node _ (VIdentifier name)) = loadIdentifier name
primaryToLLVM _ = error "Unkown type"

loadIdentifier :: String -> Codegen Operand
loadIdentifier name = do
                    variables <- ask
                    case variables Map.!? name of
                        Just v -> do
                                -- let var = LocalReference (Type.PointerType Type.double (AddrSpace 0)) (AST.Name $ fromString name)
                                load' v
                        Nothing -> valueToLLVM (VDecimalConst 42)

-------- POSTFIX

postfixToLLVM :: Node -> Codegen Operand
postfixToLLVM (Node _ (VPrimary v)) = primaryToLLVM v
postfixToLLVM _ = error "Unkown Type"

-------- EXPR

exprToLLVM :: Node -> Codegen Operand
exprToLLVM (Node _ (VExpr v ((op, v'):xs))) = binopToLLVM op v v'
exprToLLVM (Node _ (VExpr v [(op, v')])) = binopToLLVM op v v'
exprToLLVM (Node _ (VExpr v [])) = unaryToLLVM v
exprToLLVM _ = error "Unkown type"

-------- BINOP

{-
    Operator -> Value 1 -> Value 2 -> result
-}
binopToLLVM :: AST -> AST -> AST -> Codegen Operand
binopToLLVM op v v' = case nodeToVal op of
  (VBinop Data.Gt) -> gtToLLVM (nodeToValTmp v) (nodeToValTmp v')
  (VBinop Data.Lt) -> ltToLLVM (nodeToValTmp v) (nodeToValTmp v')
  (VBinop Data.Eq) -> eqToLLVM (nodeToValTmp v) (nodeToValTmp v')
  (VBinop Data.Neq) -> neqToLLVM (nodeToValTmp v) (nodeToValTmp v')
  (VBinop Data.Assign) -> assignToLLVM (nodeToValTmp v) (nodeToValTmp v')
  (VBinop op') -> opToLLVM op' (nodeToValTmp v) (nodeToValTmp v')
  _ -> error "Unknown Type"

opToLLVM :: Binop -> Value -> Value -> Codegen Operand
opToLLVM op a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br addBlock

    addBlock <- block `named` "add.start"
    res <- fct op a' b'
    return res
    where
        fct Data.Add = add
        fct Data.Sub = sub
        fct Data.Mul = mul
        fct Data.Div = sdiv
        fct _ = error "Wrong Binop"

gtToLLVM :: Value -> Value  -> Codegen Operand
gtToLLVM a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br subBlock

    subBlock <- block `named` "sub.start"
    res <- icmp Sicmp.SGT a' b'
    return res

ltToLLVM :: Value -> Value  -> Codegen Operand
ltToLLVM a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br subBlock

    subBlock <- block `named` "sub.start"
    res <- icmp Sicmp.SLT a' b'
    return res

eqToLLVM :: Value -> Value  -> Codegen Operand
eqToLLVM a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br subBlock

    subBlock <- block `named` "sub.start"
    res <- icmp Sicmp.EQ  a' b'
    return res

neqToLLVM :: Value -> Value  -> Codegen Operand
neqToLLVM a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br subBlock

    subBlock <- block `named` "sub.start"
    res <- icmp Sicmp.NE a' b'
    return res

assignToLLVM :: Value -> Value  -> Codegen Operand
assignToLLVM (VIdentifier varName) varValue = mdo
    val <- valueToLLVM varValue
    br assignBlock

    assignBlock <- block `named` "assign.start"
    ptr <- alloca Type.i32 (Just (Const.int32 1)) 0 `named` fromString varName
    store' ptr val
    let tmp = Map.insert varName ptr
    load' ptr
assignToLLVM _ _ = error "Unknown type"

------- UNOP

unaryToLLVM :: Node -> Codegen Operand
unaryToLLVM (Node _ (VUnary (Node _ (VUnop Minus)) val')) = minusToLLVM (nodeToValTmp val')
unaryToLLVM (Node _ (VUnary post (Node TNone VNothing))) = postfixToLLVM post
unaryToLLVM _ = error "Unknown type"

minusToLLVM :: Value -> Codegen Operand
minusToLLVM v = mdo
    res <- opToLLVM Data.Mul v (VDecimalConst $ -1)
    return res

-------- WHILE

-- whileToLLVM :: Value -> Codegen Operand
-- whileToLLVM v = undefined