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

import LLVM.IRBuilder as IRB
import LLVM.IRBuilder.Module

import LLVM.AST
import LLVM.AST.Type as Type

-- IMPORT CONTROL.MONAD

import Control.Monad.Reader (ReaderT (runReaderT))

-- IMPORT DATA

import qualified Data.IntMap()
import qualified Data.Map as Map

-- OTHER IMPORTS

import Prelude hiding (mod)
import qualified Control.Applicative()
import System.Process

-- OUR IMPORTS

import LLVMFunc
import Data (Value(VStmt), AST, Node (..), Codegen)


astToLLVM :: AST -> IO ()
astToLLVM instr = do
    let mod = buildModule "koakModule" $ compileModule' instr
    withContext $ \ctx ->
        withModuleFromAST ctx mod $ \mod' -> do
            withHostTargetMachine R.PIC C.Default None $ \tm -> do
                writeLLVMAssemblyToFile (LLVM.Module.File "koak.ll") mod'
                writeObjectToFile tm (LLVM.Module.File "koak.o") mod'
                callCommand "./build.sh"


compileModule' :: AST -> ModuleBuilder ()
compileModule' instr = do
    let state = Map.fromList [("test", int32 0)]
    _ <- LLVM.IRBuilder.Module.function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 $ \[_, _] -> do
        res <- runReaderT (compileInstrs instr) state
        ret res
    pure ()

compileInstrs :: AST -> Codegen Operand
compileInstrs instr = case instr of
    stmt@(Data.Node _ (VStmt _)) -> stmtToLLVM stmt
    _ -> error "ERROR"
