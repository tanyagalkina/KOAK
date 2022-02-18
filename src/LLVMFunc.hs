{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module LLVMFunc where

-- IMPORT LLVM

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
import LLVM.AST.Float
import LLVM.AST.Constant
import qualified LLVM.AST.IntegerPredicate as Sicmp
import LLVM.AST.Type as Type
-- import LLVM.AST.Global
-- import LLVM.AST.AddrSpace
-- import LLVM.AST.FloatingPointPredicate hiding (False, True)
-- import LLVM.AST.Operand as Op

import LLVM.IRBuilder as IRB
import LLVM.IRBuilder.Constant as Const
-- import LLVM.IRBuilder.Instruction
-- import LLVM.IRBuilder.Module


-- import LLVM.Prelude (traverse)

-- IMPORT CONTROL.MONAD

import Control.Monad
import Control.Monad.Reader
-- import Control.Monad.Except
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans
-- import Control.Monad.Fix
-- import Control.Monad.List (ListT)

-- IMPORT DATA

import Data.String
-- import Data.IORef
-- import Data.Int
-- import Data.Word
-- import qualified Data.Ix as Type

-- import qualified Data.ByteString.Char8 as BS
-- import Data.ByteString.Builder.Prim (condB)

-- IMPORT SYSTEM..IO

-- import System.IO
-- import System.IO.Error

-- OTHER IMPORTS

import qualified Data.Map as Map
-- import Foreign.Ptr
import Prelude hiding (mod)
-- import Numeric
-- import Text.ParserCombinators.ReadP (endBy)

-- OUT IMPORTS

import Data (Value (..), Binop (..), Type (..), AST, Node (..), Codegen, Unop (..))
import MyLLVM (store', load')
import qualified LLVM.AST as AST
import LLVM.AST.AddrSpace


-------- GET PART OF NODE

nodeToVal :: AST -> Value
nodeToVal (Node _ v) = v
nodeToVal _ = error "Error node"

nodeToType :: AST -> Data.Type
nodeToType (Node t _) = t
nodeToType _ = error "Error node"

--------- TMP

nodeToValTmp :: Node -> Value
nodeToValTmp (Node _ (VUnary (Node _ (VPostfix (Node _ (VPrimary (Node _ v@(VIdentifier _)))) _)) _)) = v
nodeToValTmp (Node _ (VUnary (Node _ (VPostfix (Node _ (VPrimary (Node _ (VLiteral (Node _ val))))) _)) _)) = val
nodeToValTmp node = error $ "error: node to value" ++  show node

-------- LITERAL

litToLLVM :: Node -> Codegen Operand
litToLLVM (Node TInteger (VDecimalConst v)) = return (int32 $ toInteger v)
litToLLVM (Node TDouble (VDoubleConst v)) =
    pure $ ConstantOperand (Float (LLVM.AST.Float.Double v))
litToLLVM n = error $"litToLLVM: Unknown type" ++ show n

valueToLLVM :: Value -> Codegen Operand
valueToLLVM (VDecimalConst v) = return (int32 $ toInteger v)
valueToLLVM (VDoubleConst v) =
    pure $ ConstantOperand (Float (LLVM.AST.Float.Double v))
valueToLLVM n = error  $ "valueToLLVM: Unknown type" ++ show n

-------- PRIMARY

primaryToLLVM :: Node -> Codegen Operand
primaryToLLVM (Node _ (VLiteral v)) = litToLLVM v
primaryToLLVM (Node _ (VIdentifier name)) = loadIdentifier name
primaryToLLVM n = error $ "primaryToLLVM: Unkown type" ++ show n

loadIdentifier :: String -> Codegen Operand
loadIdentifier name = do
                    variables <- ask
                    case variables Map.!? name of
                        Just v -> do
                                -- let var = LocalReference (Type.PointerType Type.double (AddrSpace 0)) (AST.Name $ fromString name)
                                load' v
                        -- Nothing -> valueToLLVM (VDecimalConst 42)
                        Nothing -> do -- error $ "LoadIndentifier" ++ show variables
                            let var = LocalReference (Type.PointerType Type.i32 (AddrSpace 0)) (AST.Name $ fromString "lolilol_0")
                            load var 0

-------- POSTFIX

postfixToLLVM :: Node -> Codegen Operand
postfixToLLVM (Node _ (VPrimary v)) = primaryToLLVM v
postfixToLLVM (Node TInteger (VPostfix (Node TInteger (VPrimary p )) _)) = primaryToLLVM p
postfixToLLVM n = error $ "postfixToLLVM: Unkown Type" ++ show n

-------- EXPR

exprsToLLVM :: [Node] -> Codegen Operand
exprsToLLVM [] = return $ int32 0
exprsToLLVM [x] = exprToLLVM x
exprsToLLVM (x:xs) = exprToLLVM x  >> exprsToLLVM xs


exprToLLVM :: Node -> Codegen Operand
exprToLLVM (Node _ (VExpr v ((op, v'):_))) = binopToLLVM op v v'
exprToLLVM (Node _ (VExpr v [])) = unaryToLLVM v
exprToLLVM n = error $ "exprToLLVM: Unkown type" ++ show n

-------- BINOP

{-
    Operator -> Value 1 -> Value 2 -> result
-}
binopToLLVM :: AST -> AST -> AST -> Codegen Operand
binopToLLVM op v v' = case nodeToVal op of
  (VBinop Data.Gt) -> gtToLLVM (nodeToValTmp v) (nodeToValTmp v')
  (VBinop Data.Lt) -> ltToLLVM (nodeToValTmp v) (nodeToValTmp v')
  (VBinop Data.Eq) -> eqToLLVM (nodeToValTmp v) (nodeToValTmp v')
  (VBinop Data.Neq) -> neqToLLVM (nodeToValTmp v) (nodeToValTmp v')
  (VBinop Data.Assign) -> assignToLLVM (nodeToValTmp v) (nodeToValTmp v')
  (VBinop op') -> opToLLVM op' (nodeToValTmp v) (nodeToValTmp v')
  _ -> error "binopToLLVM: Unknown Type"

opToLLVM :: Binop -> Value -> Value -> Codegen Operand
opToLLVM op a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br addBlock

    addBlock <- block `named` "opBlock"
    -- myPuts <- extern "puts" [ptr i8] i32
    -- plouf <- LLVM.IRBuilder.Instruction.call myPuts []
    res <- fct op a' b'
    -- a2 <- valueToLLVM (VDecimalConst 0)
    -- b2 <- valueToLLVM (VDecimalConst 0)
    -- test <- add a2 b2
    return res
    where
        fct Data.Add = add
        fct Data.Sub = sub
        fct Data.Mul = mul
        fct Data.Div = sdiv
        fct _ = error "Wrong Binop"

gtToLLVM :: Value -> Value  -> Codegen Operand
gtToLLVM a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br subBlock

    subBlock <- block `named` "gtBlock"
    res <- icmp Sicmp.SGT a' b'
    return res

ltToLLVM :: Value -> Value  -> Codegen Operand
ltToLLVM a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br subBlock

    subBlock <- block `named` "ltBlock"
    res <- icmp Sicmp.SLT a' b'
    return res

eqToLLVM :: Value -> Value  -> Codegen Operand
eqToLLVM a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br subBlock

    subBlock <- block `named` "eqBlock"
    res <- icmp Sicmp.EQ  a' b'
    return res

neqToLLVM :: Value -> Value  -> Codegen Operand
neqToLLVM a b = mdo
    a' <- valueToLLVM a
    b' <- valueToLLVM b
    br subBlock

    subBlock <- block `named` "neqBlock"
    res <- icmp Sicmp.NE a' b'
    return res

-- checkBind :: Codegen Operand -> Codegen Operand
-- checkBind a = do
--     a' <- ask
--     error $ "check: " ++ show a'

-- addBind :: String -> Operand -> Codegen Operand
-- addBind name ptr' = do
--     binds <- ask
--     -- pure $ Map.fromList [("lol", int32 42)]
--     -- res <- withReaderT (Map.fromList [("lol", int32 42)]) $ load' ptr'
--     local (Map.insert name ptr') $ load' ptr'
--     -- checkBind $ withReaderT (Map.insert name ptr') (load' ptr')
--     -- return $( Map.insert name ptr') binds
--     load' ptr'

assignToLLVM :: Value -> Value  -> Codegen Operand
assignToLLVM (VIdentifier varName) varValue = mdo
    val <- valueToLLVM varValue
    br assignBlock

    assignBlock <- block `named` "assign.start"
    ptr <- alloca Type.i32 (Just (Const.int32 1)) 0 `named` fromString "lolilol"
    store' ptr val
    -- res <- addBind varName ptr
    -- varia <- ask
    -- Map.insert varName ptr varia
    withReaderT (Map.insert varName ptr) $ load' ptr
    -- error $ "assign: " ++ show varia
    -- return res

-- assignToLLVM (VIdentifier varName) varValue = do
--     val <- valueToLLVM varValue
--     ptr' <- alloca Type.i32 (Just $ ConstantOperand (Int 128 4)) 8 `named` fromString varName
--     let tmp = Map.insert varName ptr
--     store ptr' 8 val
--     load ptr' 8
assignToLLVM ident val = error $ "assignToLLVM: Unknown type: " ++ show ident ++ ", " ++ show val

------- UNOP

unaryToLLVM :: Node -> Codegen Operand
unaryToLLVM (Node _ (VUnary (Node _ (VUnop Minus)) val')) =
    minusToLLVM (nodeToValTmp val')
unaryToLLVM (Node _ (VUnary post (Node TNone VNothing))) = postfixToLLVM post
unaryToLLVM _ = error "Unknown type"

minusToLLVM :: Value -> Codegen Operand
minusToLLVM v = mdo
    res <- opToLLVM Data.Mul v (VDecimalConst $ -1)
    return res

-------- WHILE

-- whileToLLVM :: Value -> Codegen Operand
-- whileToLLVM v = undefined