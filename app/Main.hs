-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecursiveDo #-}
-- {-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Prelude hiding (mod)
import System.Environment
import qualified Control.Monad.Cont

import ToLLVM (astToLLVM)
import Data (Value(VDecimalConst, VExpr, VUnary, VPostfix, VPrimary, VLiteral, VNothing, VUnop), Node (..), Type (..), Unop (Minus))
import SrcManager

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> repl
        files -> Control.Monad.Cont.void (processFiles files)

