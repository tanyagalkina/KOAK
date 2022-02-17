-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecursiveDo #-}
-- {-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import SrcManager  
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
      []      -> repl
      files -> processFiles files >> return ()

