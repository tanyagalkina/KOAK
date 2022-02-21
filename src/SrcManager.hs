{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module SrcManager where

import Prelude hiding (mod)
import Data.ByteString.Short
import qualified LLVM.AST as AST
import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad (mfilter)
import qualified Data.Maybe

import CreateAST
import Parser
import ToLLVM (astToLLVM)
import Data (Node (Error))


-- exampleExpr :: Node
-- exampleExpr = Node TInteger (VExprs [Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier "i")))) (Node TNone VNothing))) (Node TNone VNothing))) [(Node TNone (VBinop Assign),Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VLiteral (Node TInteger (VDecimalConst 21)))))) (Node TNone VNothing))) (Node TNone VNothing)))]),Node TInteger (VExpr (Node TInteger (VUnary (Node TInteger (VPostfix (Node TInteger (VPrimary (Node TInteger (VIdentifier "i")))) (Node TNone VNothing))) (Node TNone VNothing))) [])])

-- create Module

emptyModule :: ShortByteString -> AST.Module
emptyModule label = AST.defaultModule {AST.moduleName = label}

initErrorModule:: AST.Module
initErrorModule = emptyModule "errorModule"

initModule :: AST.Module
initModule = emptyModule "koakModule"

-- process

process :: AST.Module -> String -> IO AST.Module
process _ source = do
  let res = runParser createAST source
  case res of
      Nothing -> putStrLn "ERROR : Syntax Error" >> return initErrorModule
      Just (Error s, _) -> putStr "ERROR : " >> putStrLn s
                                               >> return initErrorModule
      Just (ast, _) -> astToLLVM ast >> return initModule

-- process files

-- ADD ERROR HANDLING ?

processFiles :: [String] -> IO AST.Module
processFiles fnames = concatSources "" fnames >>= process initModule

concatSources :: String -> [String] -> IO String
concatSources base [] = return base
concatSources base (x:xs) = do
    src <- readFile x
    let cleanSource = replace '\n' ' ' src
    concatSources (base ++ cleanSource) xs

replace :: Eq b => b -> b -> [b] -> [b]
replace a b = map $ Data.Maybe.fromMaybe b . mfilter (/= a) . Just

repl :: IO ()
repl = putStrLn "KOAK Version 1.0.0\nCopyright 2021-2022 Epitech Roazhon, Inc."
            >> runInputT defaultSettings (loop initModule)
    where
        loop mod = do
            maybeInput <- getInputLine "koak> "
            case maybeInput of
                Nothing -> outputStrLn "Goodbye."
                Just input -> do
                    newModule <- liftIO $ process mod input
                    loop newModule