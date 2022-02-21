module Main where

import Prelude hiding (mod)
import System.Environment ( getArgs )
import qualified Control.Monad.Cont

import ToLLVM (astToLLVM)
import SrcManager ( processFiles, repl )

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> repl
        files -> Control.Monad.Cont.void (processFiles files)