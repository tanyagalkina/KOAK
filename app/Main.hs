-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecursiveDo #-}
-- {-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Prelude hiding (mod)
import System.Environment
import qualified Control.Monad.Cont

import ToLLVM (astToLLVM)
import Data (Value(VDecimalConst, VExpr, VUnary, VPostfix, VPrimary, VLiteral, VNothing, VUnop), Node (..), Type (..), Unop (Minus))
import Data (Value(VDecimalConst, VExpr, VUnary, VPostfix, VPrimary, VLiteral, VNothing, VUnop, VExprs, VIdentifier, VBinop), Node (..), Type (..), Unop (Minus), Binop (Assign))
import SrcManager

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> repl
        files -> Control.Monad.Cont.void (processFiles files)


-- main :: IO ()
-- -2;
-- main = astToLLVM $ Node TInteger (VExpr (Node TInteger (VUnary (Node TNone (VUnop Minus)) (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 2)))))) (Node TNone VNothing))) (Node TNone VNothing))))) [])
-- 6 + 2;
-- main = astToLLVM $ Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 6)))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 2)))))) (Node TNone VNothing))) (Node TNone VNothing)))])
-- i = 1:1;
-- Just (Node (TError "fromList [(\"i\",TInteger)]") (VStmt [Node TNone (VKdefs (Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier "i")))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 1)))))) (Node TNone VNothing))) (Node TNone VNothing)))]),Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier "i")))) (Node TNone VNothing))) (Node TNone VNothing))) [])])))]),"")
-- main = astToLLVM $ Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier "i")))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 21)))))) (Node TNone VNothing))) (Node TNone VNothing)))]),Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier "i")))) (Node TNone VNothing))) (Node TNone VNothing))) [])])
