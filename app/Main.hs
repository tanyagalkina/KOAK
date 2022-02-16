{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

-- import qualified Data.Map.Strict as Map
import Prelude hiding (mod)
import System.Environment
import LLVM

-----------
import ToLLVM (astToLLVM)
import Data (Value(VDoubleConst, VDecimalConst, VExpr, VUnary, VPostfix, VPrimary, VLiteral, VNothing, VBinop, VUnop), Node (..), Type (..), Binop (Add, Sub, Mul, Div, Gt), Unop (Minus))

-- main :: IO ()
-- main = astToLLVM $ Node TInteger (VExpr (Node TInteger (VUnary (Node TNone (VUnop Minus)) (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 2)))))) (Node TNone VNothing))) (Node TNone VNothing))))) [])
-- -- main = astToLLVM $ Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 6)))))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Add),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 2)))))) (Node TNone VNothing))) (Node TNone VNothing)))])


main :: IO ()
main = do
  args <- getArgs
  case args of
      -- [] -> putStrLn "You did not say anything!"
      -- [path] -> putStrLn path
      []      -> repl
      [files] -> processFiles files >> return ()


-- emptyModule :: ShortByteString -> AST.Module
-- emptyModule label = defaultModule {moduleName = label}

-- initErrorModule:: AST.Module
-- initErrorModule = emptyModule "my error koak"

-- initModule :: AST.Module
-- initModule = emptyModule "koakKoakKoak"

process :: AST.Module -> String -> IO AST.Module
process modo source = do
  let res = traceShow ("you gave:: " ++ source) runParser parseStmt source 
  case res of 
      Nothing -> print "Syntax Error" >> return initErrorModule
    --Left err -> print err >> return initErrorModule
    --Right ex -> return initModule
      Just ex -> print (head $ (fst ex)) >> return initErrorModule
    -- Right ex -> do
    --   ast <- codegen modo ex
    --    return $ ast


-- maybe add error handling ?
processFile :: String -> IO AST.Module
processFile fname = readFile fname >>= process initModule


repl :: IO ()
repl = putStrLn 
    "KOAK Version 1.0.0\nCopyright 2021-2022 Epitech Roazhon, Inc."
     >> runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    minput <- getInputLine "koak> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- liftIO $ process mod input
        loop modn

