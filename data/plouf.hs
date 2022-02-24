{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module MyLLVM where

-- IMPORT LLVM

import LLVM.IRBuilder as IRB
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
-- import LLVM.AST.Constant
-- import LLVM.AST.Global
-- import LLVM.AST.AddrSpace
-- import LLVM.AST.Float
-- import LLVM.AST.FloatingPointPredicate hiding (False, True)
-- import qualified LLVM.AST.IntegerPredicate as Sicmp
-- import LLVM.AST.Operand as Op
-- import LLVM.AST.Type as Type

-- IMPORT CONTROL.MONAD

-- import Control.Monad.Reader
-- import Control.Monad
-- import Control.Monad.IO.Class
-- import Control.Monad.Except
-- import Control.Monad.Trans
-- import Control.Monad.Fix
-- import Control.Monad.List (ListT)

-- IMPORT DATA

import Data.String()
-- import Data.IORef
-- import Data.Int
-- import qualified Data.ByteString.Char8 as BS
-- import Data.Word
-- import qualified Data.Ix as Type
--import Data.ByteString.Builder.Prim (condB)

-- IMPORT SYSTEM.IO

-- import System.IO
-- import System.IO.Error ()

-- OTHER IMPORTS

import qualified Data.Map as Map()
import Prelude hiding (mod)
-- import Foreign.Ptr
-- import Data (ArgsType (Int, Double, Void))
-- import Numeric
-- import Text.ParserCombinators.ReadP (endBy)

-- OUR IMPORTS

import Data (Codegen)

-- type Binds = Map.Map String Operand


-- LLVM helpers

-- data ArgsType = Int | Double | Void


-- typeToLType :: ArgsType -> LType
-- typeToLType Double = LType.float
-- typeToLType Int = LType.i32
-- typeToLType _ = error "Unkown type"

-- type AssignedValues = Map.Map String Operand

-- type Codegen = ReaderT AssignedValues (IRBuilderT ModuleBuilder)


load' :: Operand -> Codegen Operand
load' adr = load adr 0

store' :: Operand -> Operand -> Codegen ()
store' adr = store adr 0

allocate :: LLVM.AST.Type -> Operand -> Codegen Operand
allocate ty val = do
    adr <- alloca ty (Just (IRB.int32 1)) 0
    store' adr val
    pure adr

--

{-
    generateIR : take our AST and return the IR
-}
-- generateIR :: Value -> IRBuilder ()
-- generateIR val = case val of
--     VExpr -> _


if' :: Operand -> IRBuilder a -> IRBuilder ()
if' cond instr = mdo
    condBr cond ifBlock end

    ifBlock <- block `named` "if.start"
    _ <- instr
    br end

    end <- block `named` "if.end"
    pure ()

-- exampleIF :: Operand -> IRBuilder ()
-- exampleIF bool =
--     if' bool $ do
--         ret (Type.int8 42)

-- infinite loop

infLoop :: IRBuilder a -> IRBuilder ()
infLoop instr = mdo
    loopBlock <- block `named` "loop.start"
    _ <- instr
    br loopBlock

while' :: Operand -> IRBuilder a -> IRBuilder ()
while' cond instr = mdo
    condBr cond ifBlock end

    ifBlock <- block `named` "if.start"
    _ <- instr
    condBr cond ifBlock end

    end <- block `named` "if.end"
    pure ()

-- for (int i = 0; i < 10; i++) {...}

-- for' :: Operand                         -- start value
--     -> (Operand -> IRBuilder Operand)   -- condition check
--     -> (Operand -> IRBuilder Operand)   -- loop action (i++)
--     -> (Operand -> IRBuilder a)         -- body instruction
--     -> iRBuilder ()
-- for' val cond act instr = mdo
--     start <- currentBlock
--     br begin

--     begin <- block `named` "for.begin"=
--     loopVal <- phi [(val, start), (updatedVal, bodyEnd)]
--     res <- cond loopVal
--     condBr res bodyStart end

--     bodyStart <- block `named` "for.body"
--     _ <- instr loopVal
--     updatedVal <- act loopVal
--     bodyEnd <- currentBlock
--     br begin

--     end <- block `named` "for.end"
--     pure ()

-- data Binop = Mul | Div | Add | Sub | Gt | Lt | Eq | Neq | Assign


-- myBinop :: Binop -> Operand -> Operand -> IRBuilder ()
-- myBinop Add = myAdd
-- myBinop Sub = mySub
-- myBinop Div = myDiv
-- myBinop Mul = myMul

-- Binop

-- myDiv :: Operand -> Operand -> IRBuilder Operand
-- myDiv a b = div a b
