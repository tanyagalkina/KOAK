{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use empty" #-}
module ToLLVM where

-- IMPORT LLVM

import LLVM.CodeGenOpt
import LLVM.Context
import LLVM.Module
import LLVM.Target
import LLVM.Relocation as R
import LLVM.CodeModel as C
-- import LLVM.IRBuilder.Instruction

import LLVM.IRBuilder as IRB
import LLVM.IRBuilder.Module

import LLVM.AST
import LLVM.AST.Type as Type

-- IMPORT CONTROL.MONAD

import Control.Monad.Reader (ReaderT (runReaderT))
-- import Control.Monad.Trans

-- IMPORT DATA

import qualified Data.IntMap()
import qualified Data.Map as Map

-- OTHER IMPORTS

import Prelude hiding (mod)
import qualified Control.Applicative()
import System.Process
-- import Debug.Trace

-- OUR IMPORTS

import LLVMFunc as F
import Data (Value(VDecimalConst, VDoubleConst, VExpr), AST, Node (..), Codegen)


astToLLVM :: AST -> IO ()
astToLLVM instr = do
    let mod = buildModule "koakModule" $ compileModule' instr
    withContext $ \ctx ->
        withModuleFromAST ctx mod $ \mod' -> do
        let opt = None
        withHostTargetMachine R.PIC C.Default opt $ \tm -> do
            writeLLVMAssemblyToFile (LLVM.Module.File "my.ll") mod' -- generates an IR file
            writeObjectToFile tm (LLVM.Module.File "my.o") mod' -- builds an object file
            callCommand "gcc my.o"
            callCommand "./a.out"
            callCommand "echo $?"
      

compileModule' :: AST -> ModuleBuilder ()
compileModule' instr = do
    let state = Map.fromList [("test", int32 0)]
    _ <- LLVM.IRBuilder.Module.function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 $ \[_, _] -> do
        res <- runReaderT (compileInstrs instr) state
        ret res
    pure ()

compileInstrs :: AST -> Codegen Operand
-- compileInstrs instr = traverse_ compInstr where
compileInstrs instr = case instr of
    (Data.Node _ v@(VDecimalConst _)) -> F.valueToLLVM v
    (Data.Node _ v@(VDoubleConst _)) -> F.valueToLLVM v
    n@(Data.Node _ (VExpr _ _)) -> F.exprToLLVM n
    _ -> error "Unknown value"
