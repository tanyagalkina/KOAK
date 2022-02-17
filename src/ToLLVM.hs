{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use empty" #-}
module ToLLVM where

import LLVM.AST
import LLVM.CodeGenOpt
import LLVM.Context
import LLVM.Module
import LLVM.Target
import Prelude hiding (mod)


-- import LLVM.IRBuilder.Instruction

---------------

import LLVM.AST.Type as Type
import LLVM.IRBuilder as IRB

import Control.Monad.Reader (ReaderT (runReaderT))


-----------
import LLVM.IRBuilder.Module
import LLVM.Relocation as R
import LLVM.CodeModel as C

import LLVMFunc as F
import Data (Value(VDecimalConst, VDoubleConst, VExpr), AST, Node (..), Codegen)
import qualified Control.Applicative()
import qualified Data.IntMap()
import qualified Data.Map as Map

astToLLVM :: AST -> IO ()
astToLLVM instr = do
  let mod = buildModule "mymod" $ compileModule' instr

  withContext $ \ctx ->
    withModuleFromAST ctx mod $ \mod' -> do
    let opt = None
    withHostTargetMachine R.PIC C.Default opt $ \tm -> do
      writeLLVMAssemblyToFile (LLVM.Module.File "my.ll") mod'
      writeObjectToFile tm (LLVM.Module.File "my.o") mod'

compileModule' :: AST -> ModuleBuilder ()
compileModule' instr = do
  let state = Map.fromList [("test", int32 0)]
  _ <- LLVM.IRBuilder.Module.function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 $ \[_, _] -> do
    res <- runReaderT (compileInstrs instr) state
    pure ()
  pure ()

compileInstrs :: AST -> Codegen Operand
-- compileInstrs instr = traverse_ compInstr where
compileInstrs instr = case instr of
    (Data.Node _ v@(VDecimalConst _)) -> F.valueToLLVM v
    (Data.Node _ v@(VDoubleConst _)) -> F.valueToLLVM v
    n@(Data.Node _ (VExpr _ _)) -> F.vExprToLLVM n
    _ -> error "Unknown val"
