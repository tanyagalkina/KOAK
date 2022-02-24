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
-- import LLVM.IRBuilder.Module
-- import LLVM.IRBuilder.Instruction

-- import LLVM.AST
-- import LLVM.AST.Type as Type

-- IMPORT CONTROL.MONAD

-- import Control.Monad.Reader (ReaderT (runReaderT))

-- IMPORT DATA

import qualified Data.IntMap()
-- import qualified Data.Map as Map

-- OTHER IMPORTS

import Prelude hiding (mod)
import qualified Control.Applicative()
import System.Process

-- OUR IMPORTS

import LLVMFunc
import Data (Value(VStmt), AST, Node (..))


astToLLVM :: AST -> IO ()
astToLLVM instr = do
    callCommand "./script/clear.sh"
    let mod = buildModule "koakModule" $ compileModule instr
    withContext $ \ctx ->
        withModuleFromAST ctx mod $ \mod' -> do
            withHostTargetMachine R.PIC C.Default None $ \tm -> do
                writeLLVMAssemblyToFile (LLVM.Module.File "koak.ll") mod'
                writeObjectToFile tm (LLVM.Module.File "koak.o") mod'
                callCommand "./script/build.sh"


compileModule :: AST -> ModuleBuilder ()
compileModule instr = case instr of
    stmt@(Data.Node _ (VStmt _)) -> stmtToLLVM stmt (int32 0)
    _ -> error "ERROR"
