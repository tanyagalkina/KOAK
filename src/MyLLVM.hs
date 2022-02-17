{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module MyLLVM where

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
import Control.Monad.Reader
import qualified Data.Map as Map




-- type Binds = Map.Map String Operand


-- LLVM helpers

-- data ArgsType = Int | Double | Void


-- typeToLType :: ArgsType -> LType
-- typeToLType Double = LType.float
-- typeToLType Int = LType.i32
-- typeToLType _ = error "Unkown type"

type AssignedValues = Map.Map String Operand

type Codegen = ReaderT AssignedValues (IRBuilderT ModuleBuilder)


load' :: Operand -> Codegen Operand
load' adr = load adr 0

store' :: Operand -> Operand -> Codegen ()
store' adr val = store adr 0 val

allocate :: Type -> Operand -> Codegen Operand
allocate ty val = do
    adr <- alloca ty (Just (Type.int32 1)) 0
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
    instr
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
myAdd :: Operand -> Operand -> IRBuilder Operand
myAdd a b = add a b

mySub :: Operand -> Operand -> IRBuilder Operand
mySub a b = sub a b

myMul :: Operand -> Operand -> IRBuilder Operand
myMul a b = mul a b

-- myDiv :: Operand -> Operand -> IRBuilder Operand
-- myDiv a b = div a b


