{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

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


int :: Type
int = IntegerType 32

-- if' :: Operand -> IRBuilder a -> IRBuilder ()
-- if' cond instr = mdo
--     condBr cond ifBlock end

--     ifBlock <- block `named` "if.start"
--     instr
--     br end

--     end <- block `named` "if.end"
--     pure ()

-- myAdd :: Operand -> Operand  -> IRBuilder Operand
-- myAdd a b = mdo
--     br addBlock

--     addBlock <- block `named` "add.start"
--     res <- fadd a b
--     return res


-- defAdd :: Definition
-- defAdd = GlobalDefinition functionDefaults
--   { LLVM.AST.Global.name = Name "add"
--   , parameters =
--       ( [ Parameter int (Name "a") []
--         , Parameter int (Name "b") [] ]
--       , False )
--   , returnType = int
--   , basicBlocks = [body]
--   }
--   where
--     body = BasicBlock
--         (Name "entry")
--         [ Name "result" :=
--             Add False  -- no signed wrap
--                 False  -- no unsigned wrap
--                 (LocalReference int (Name "a"))
--                 (LocalReference int (Name "b"))
--                 []]
--         (Do $ Ret (Just (LocalReference int (Name "result"))) [])


-- module_ :: AST.Module
-- module_ = defaultModule
--   { moduleName = "basic"
--   , moduleDefinitions = [defAdd]
--   }


-- toLLVM :: AST.Module -> IO ()
-- toLLVM mod = withContext $ \ctx -> do
--   llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
--   BS.putStrLn llvm


data CompilerState = CompilerState {
  val :: Int,
  x :: Int
}

type Codegen = ReaderT CompilerState (IRBuilderT ModuleBuilder)

toLLVM' :: Int -> IO ()
toLLVM' instr = do
  let mod = buildModule "mymod" $ compileModule' instr

  withContext $ \ctx -> do
    withModuleFromAST ctx mod $ \mod' -> do
      let opt = None
      withHostTargetMachine R.PIC C.Default opt $ \tm -> do
        writeLLVMAssemblyToFile (LLVM.Module.File "my.ll") mod'
        writeObjectToFile tm (LLVM.Module.File "my.o") mod'

compileModule' :: Int -> ModuleBuilder ()
compileModule' instr = do
  let state = CompilerState 1 0
  _ <- LLVM.IRBuilder.Module.function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 $ \[argc, argv] -> do
    _ <- runReaderT (compileInstrs instr) state
    ret (int32 0)
  pure ()

compileInstrs ::Int -> Codegen ()
-- compileInstrs instr = traverse_ compInstr where
compileInstrs instr = case instr of
    0 -> myAdd (int32 10) (int32 2)
    _ -> myAdd (int32 40) (int32 2)

-- myAdd :: Codegen Operand -> Codegen Operand  -> Codegen ()
myAdd :: Operand -> Operand  -> Codegen ()
myAdd a' b' = mdo
    -- a' <- a
    -- b' <- b
    br addBlock

    addBlock <- block `named` "add.start"
    res <- fadd a' b'
    pure ()

main :: IO ()
main = toLLVM' 0