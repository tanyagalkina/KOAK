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

addToLLVM :: Operand -> Operand  -> Codegen Operand
addToLLVM a' b' = mdo
    -- a' <- a
    -- b' <- b
    br addBlock

    addBlock <- block `named` "add.start"
    res <- fadd (int32 42) (int32 42)
    return res

subbToLLVM :: Operand -> Operand  -> Codegen Operand
subbToLLVM a' b' = mdo
    -- a' <- a
    -- b' <- b
    br subBlock

    subBlock <- block `named` "sub.start"
    res <- fadd (int32 42) (int32 42)
    return res
