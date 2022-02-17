{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module LLVMFunc where

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Int
-- import qualified Data.Map.Strict as Map
import Foreign.Ptr
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.CodeGenOpt
import LLVM.CodeModel
import LLVM.Context
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Module
import LLVM.OrcJIT
import LLVM.Relocation
import LLVM.Target
import Prelude hiding (mod)

import Data (ArgsType (Int, Double, Void), Unop (Minus), Codegen, BinopFct)


import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except
import Data.ByteString.Char8 as BS
import LLVM.IRBuilder (IRBuilder)
import LLVM.IRBuilder.Instruction

---------------

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Int
-- import qualified Data.Map.Strict as Map
import Foreign.Ptr
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.CodeGenOpt
import LLVM.CodeModel
import LLVM.Context
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Module
import LLVM.OrcJIT
import LLVM.Relocation
import LLVM.Target
import Prelude hiding (mod)

import Data (ArgsType (Int, Double, Void))


import Control.Monad
import Control.Monad.IO.Class
import Data.String
import Foreign.Ptr
import qualified LLVM.AST as AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.AST.Float
import LLVM.AST.FloatingPointPredicate hiding (False, True)
import qualified LLVM.AST.IntegerPredicate as Sicmp
import LLVM.AST.Operand as Op
import LLVM.AST.Type as Type
import LLVM.IRBuilder as IRB
import LLVM.Module
import LLVM.PassManager
import LLVM.Pretty
import LLVM.Target
import System.IO
import System.IO.Error
import Data.Int
import Data.Word
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Fix
import LLVM.Target
import LLVM.CodeModel
import Numeric


import LLVM.IRBuilder (IRBuilder)
import Control.Monad.List (ListT)
import qualified Data.Ix as Type
import qualified LLVM.IRBuilder as Type
import Data.ByteString.Builder.Prim (condB)
import Text.ParserCombinators.ReadP (endBy)
import Control.Monad.Reader (ReaderT (runReaderT))


-----------
import LLVM.IRBuilder.Module
import LLVM.Relocation as R
import LLVM.CodeModel as C
import LLVM.Prelude (traverse_)
import qualified LLVM.AST as LLVM

import LLVM.IRBuilder.Constant as Const


import Control.Monad
import Control.Monad.IO.Class
import Data.String
import Foreign.Ptr
import qualified LLVM.AST as AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.AST.Float
import LLVM.AST.FloatingPointPredicate hiding (False, True)
import qualified LLVM.AST.IntegerPredicate as Sicmp
import LLVM.AST.Operand as Op
import LLVM.AST.Type as Type
import LLVM.IRBuilder as IRB
import LLVM.Module
import LLVM.PassManager
import LLVM.Pretty
import LLVM.Target
import System.IO
import System.IO.Error
import Data.Int
import Data.Word
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Fix
import LLVM.Target
import LLVM.CodeModel
import Numeric

------------------
import Data (Value (..), Binop (..), Type (..), AST, Node (..), Codegen)
import qualified Data.Map as Map
import MyLLVM (store', load',)
-- import Sicmp.IntegerPredicate
import Control.Monad.Reader


astToVal :: AST -> Value
astToVal (Node _ v) = v

astToType :: AST -> Data.Type
astToType (Node t _) = t


nodeToVal :: Node -> Value
nodeToVal (Node _ (VUnary (Node _ (VPostfix (Node _ (VPrimary (Node _ (VLiteral (Node _ value))))) _)) _)) = value
nodeToVal _ = error "error: node to value"



valueToLLVM :: Value -> Codegen Operand
valueToLLVM (VDecimalConst v) = return (int32 $ toInteger v)
-- valueToLLVM (VDoubleConst v) = return  (Const.double v)
-- valueToLLVM (VDecimalConst v) = pure $ ConstantOperand (Float (LLVM.AST.Float.Double (fromIntegral v)))
valueToLLVM (VDoubleConst v) = pure $ ConstantOperand (Float (LLVM.AST.Float.Double v))
valueToLLVM _ = error "Unknown type"


litToLLVM :: Node -> Codegen Operand
litToLLVM (Node TInteger v) = valueToLLVM v
litToLLVM (Node TDouble  v) = valueToLLVM v
litToLLVM _ = error "Unknown type"


loadIdentifier :: String -> Codegen Operand
loadIdentifier name = do
                    variables <- ask
                    case variables Map.!? name of
                        Just v -> do
                                -- let var = LocalReference (Type.PointerType Type.double (AddrSpace 0)) (AST.Name $ fromString name)
                                load' v
                        Nothing -> valueToLLVM (VDecimalConst 42)

primaryToLLVM :: Node -> Codegen Operand
primaryToLLVM (Node _ (VLiteral v)) = litToLLVM v
primaryToLLVM (Node _ (VIdentifier name)) = loadIdentifier name
primaryToLLVM _ = error "Unkown type"

postfixToLLVM :: Node -> Codegen Operand
postfixToLLVM (Node _ (VPrimary v)) = primaryToLLVM v
postfixToLLVM _ = error "Unkown Type"

{-
  process our Ast for VExpr
-}

vExprToLLVM :: Node -> Codegen Operand
vExprToLLVM (Node _ (VExpr v ((op, v'):xs))) = binopToLLVM op v v'
vExprToLLVM (Node _ (VExpr v [(op, v')])) = binopToLLVM op v v'
vExprToLLVM (Node _ (VExpr v [])) = unaryToLLVM v
vExprToLLVM _ = error "Unkown type"


-------- BINOP

{-
  Operator -> Value 1 -> Value 2 -> result
-}
binopToLLVM :: AST -> AST -> AST -> Codegen Operand
binopToLLVM op v v' = case astToVal op of
  (VBinop Data.Gt) -> gtToLLVM (nodeToVal v) (nodeToVal v')
  (VBinop Data.Lt) -> ltToLLVM (nodeToVal v) (nodeToVal v')
  (VBinop Data.Eq) -> eqToLLVM (nodeToVal v) (nodeToVal v')
  (VBinop Data.Neq) -> neqToLLVM (nodeToVal v) (nodeToVal v')
  (VBinop Data.Assign) -> assignToLLVM (nodeToVal v) (nodeToVal v')
  (VBinop op') -> opToLLVM op' (nodeToVal v) (nodeToVal v')
  _ -> error "Unknown Type"

opToLLVM :: Binop -> Value -> Value -> Codegen Operand
opToLLVM op a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br addBlock

    addBlock <- block `named` "add.start"
    res <- (fct op) a' b'
    return res
    where
      fct Data.Add = add
      fct Data.Sub = sub
      fct Data.Mul = mul
      fct Data.Div = sdiv

condToLLVM :: BinopFct -> Value -> Value -> Codegen Operand
condToLLVM fct a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br addBlock

    addBlock <- block `named` "add.start"
    res <- fct a' b'
    return res

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

-- Node (TError "fromList []") (VStmt [Node TNone (VKdefs (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])])))])
-- Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))
-- (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))

-- ASSIGN

-- Node (TError "fromList [(\"i\",TInteger)]") (VStmt [Node TNone (VKdefs (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier "i")))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])])))])
-- [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier "i")))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])]

assignToLLVM :: Value -> Value  -> Codegen Operand
assignToLLVM (VIdentifier varName) varValue = mdo
    val <- valueToLLVM varValue
    br assignBlock

    assignBlock <- block `named` "assign.start"
    ptr <- alloca Type.i32 (Just (Type.int32 1)) 0 `named` (fromString varName)
    store' ptr val
    let tmp = Map.insert varName ptr
    load' ptr
assignToLLVM _ _ = error "Unknown type"


-- fromAssignToLLVM :: Expr -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
-- fromAssignToLLVM (Assign (Var nameS _) xpr xtype)    = do
--                                                 let Type = exprTypeToType xtype
--                                                 val <- fromExprsToLLVM [xpr]
--                                                 ptr <- alloca Type (Just $ ConstantOperand (Int 128 4)) (8 :: Word32) `named` (fromString nameS)
--                                                 let tmp = Map.insert nameS ptr
--                                                 store ptr 8 val
--                                                 retn <- load ptr 8
--                                                 return retn

------- UNOP

unaryToLLVM :: Node -> Codegen Operand
unaryToLLVM (Node _ (VUnary (Node _ (VUnop Minus)) val')) = minusToLLVM (nodeToVal val')
unaryToLLVM (Node _ (VUnary post (Node TNone VNothing))) = postfixToLLVM post
unaryToLLVM _ = error "Unknown type"

minusToLLVM :: Value -> Codegen Operand
minusToLLVM v = mdo
    res <- opToLLVM Data.Mul v (VDecimalConst $ -1)
    return res


-- Node (TError "fromList []") (VStmt [Node TNone (VKdefs (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TNone (VUnop Minus)) (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))))) [])])))])
-- (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TNone (VUnop Minus)) (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))))) [])])
-- (VExpr (Node TInteger (VUnary (Node TNone (VUnop Minus)) (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))))) [])

---- LOGICAL GATES

-- whileToLLVM :: Value -> Codegen Operand
-- whileToLLVM v = undefined