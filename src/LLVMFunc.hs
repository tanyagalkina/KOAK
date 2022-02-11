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

import Data (ArgsType (Int, Double, Void))


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
import Data (Value (..), Binop (..), Type (..), AST, Node (..))
-- import Sicmp.IntegerPredicate

-- Where store info to get through out theAST
data CompilerState = CompilerState {
  val :: Int,
  x :: Int
}

type Codegen = ReaderT CompilerState (IRBuilderT ModuleBuilder)

astToVal :: AST -> Value
astToVal (Node _ v) = v

astToType :: AST -> Data.Type
astToType (Node t _) = t

valueToLLVM :: Value -> Codegen Operand
valueToLLVM (VDecimalConst v) = return (int32 $ toInteger v)
-- valueToLLVM (VDoubleConst v) = return  (Const.double v)
-- valueToLLVM (VDecimalConst v) = pure $ ConstantOperand (Float (LLVM.AST.Float.Double (fromIntegral v)))
valueToLLVM (VDoubleConst v) = pure $ ConstantOperand (Float (LLVM.AST.Float.Double v))
valueToLLVM _ = error "Unknown type"

{-
  process our Ast for VExpr
-}

vExprToLLVM :: Node -> Codegen Operand
vExprToLLVM (Node _ (VExpr v ((op, v'):xs))) = binopToLLVM op v v'
vExprToLLVM (Node _ (VExpr v [(op, v')])) =binopToLLVM op v v'
vExprToLLVM _ = error "Unkown type"


{-
  Operator -> Value 1 -> Value 2 -> result
-}

-- binopToLLVM (VBinop Data.Add) = addToLLVM (VDecimalConst 42) (VDecimalConst 42)
-- binopToLLVM (VBinop Data.Sub) = subToLLVM (VDecimalConst 42) (VDecimalConst 42)
-- binopToLLVM _ = error "Unknown Binop"
binopToLLVM :: AST -> AST -> AST -> Codegen Operand
binopToLLVM op v v' = case astToVal op of
  (VBinop Data.Add) -> addToLLVM (nodeToVal v) (nodeToVal v')
  (VBinop Data.Sub) -> subToLLVM (nodeToVal v) (nodeToVal v')
  (VBinop Data.Mul) -> mulToLLVM (nodeToVal v) (nodeToVal v')
  (VBinop Data.Div) -> divToLLVM (nodeToVal v) (nodeToVal v')
  (VBinop Data.Gt) -> gtToLLVM (nodeToVal v) (nodeToVal v')
  (VBinop Data.Lt) -> ltToLLVM (nodeToVal v) (nodeToVal v')
  (VBinop Data.Eq) -> eqToLLVM (nodeToVal v) (nodeToVal v')
  (VBinop Data.Neq) -> neqToLLVM (nodeToVal v) (nodeToVal v')



{-
  get to value and return it as llvm
-}

addToLLVM :: Value -> Value -> Codegen Operand
addToLLVM a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br addBlock

    addBlock <- block `named` "add.start"
    res <- add a' b'
    return res

subToLLVM :: Value -> Value  -> Codegen Operand
subToLLVM a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br subBlock

    subBlock <- block `named` "sub.start"
    res <- sub a' b'
    return res

mulToLLVM :: Value -> Value  -> Codegen Operand
mulToLLVM a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br subBlock

    subBlock <- block `named` "sub.start"
    res <- mul a' b'
    return res

divToLLVM :: Value -> Value  -> Codegen Operand
divToLLVM a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br subBlock

    subBlock <- block `named` "sub.start"
    res <- sdiv a' b'
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


nodeToVal :: Node -> Value
nodeToVal (Node _ (VUnary (Node _ (VPostfix (Node _ (VPrimary (Node _ (VLiteral (Node _ value))))) _)) _)) = value
nodeToVal _ = error "error: node to value"

-- Node (TError "fromList []") (VStmt [Node TNone (VKdefs (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))])])))])
-- Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))
-- (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))