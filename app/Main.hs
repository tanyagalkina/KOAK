{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

-- import qualified Data.Map.Strict as Map
import Prelude hiding (mod)


---------------

-- import qualified Data.Map.Strict as Map
import Prelude hiding (mod)

-----------
import ToLLVM (astToLLVM)
import Data (Value(VDoubleConst, VDecimalConst), Node (..), Type (..))

main :: IO ()
main = astToLLVM (Node (TInteger TNone) (VDecimalConst 2))