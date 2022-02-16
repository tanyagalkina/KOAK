{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module ToLLVM where

import Data.Int
import LLVM.AST
import LLVM.CodeGenOpt
import LLVM.Context
import LLVM.Module
import LLVM.Target
import Prelude hiding (mod)


import LLVM.IRBuilder.Instruction

---------------

-- import qualified Data.Map.Strict as Map
import Prelude hiding (mod)

import LLVM.AST.Type as Type
import LLVM.IRBuilder as IRB

import Control.Monad.Reader (ReaderT (runReaderT))


-----------
import LLVM.IRBuilder.Module
import LLVM.Relocation as R
import LLVM.CodeModel as C
import Debug.Trace
import Control.Monad.Trans

import LLVMFunc as F
import System.Process
import Data (Value(VDecimalConst, VDoubleConst, VExpr), AST, Node (..))

data CompilerState = CompilerState {
  val :: Int,
  x :: Int
}

type Codegen = ReaderT F.CompilerState (IRBuilderT ModuleBuilder)

astToLLVM :: AST -> IO ()
astToLLVM instr = do
  let mod = traceShow ("TOLLVM HAS GOT::\n", instr) buildModule "mymod" $ compileModule' instr

  withContext $ \ctx ->
    withModuleFromAST ctx mod $ \mod' -> do
    let opt = None
    withHostTargetMachine R.PIC C.Default opt $ \tm -> do
      
      writeLLVMAssemblyToFile (LLVM.Module.File "my.ll") mod' -- generates an IR file
      writeObjectToFile tm (LLVM.Module.File "my.o") mod' -- builds an object file
      callCommand "clang my.ll -o my.bc"    -- builds a binary
      

compileModule' :: AST -> ModuleBuilder ()
compileModule' instr = do
  let state = F.CompilerState 1 0
  _ <- LLVM.IRBuilder.Module.function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 $ \[argc, argv] -> do
    res <- runReaderT (compileInstrs instr) state
    ret res
  pure ()

compileInstrs :: AST -> F.Codegen Operand
-- compileInstrs instr = traverse_ compInstr where
compileInstrs instr = case instr of
    (Data.Node _ v@(VDecimalConst _)) -> F.valueToLLVM v
    (Data.Node _ v@(VDoubleConst _)) -> F.valueToLLVM v
    n@(Data.Node _ v@(VExpr _ _)) -> F.vExprToLLVM n
    -- _ -> error "Unknown val"
    _ -> F.valueToLLVM (VDecimalConst 42)
