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
import Control.Exception
import qualified Data.Maybe

import CreateAST
import Parser
import ToLLVM (astToLLVM)
import Data (Node (Error))

-- create Module

emptyModule :: ShortByteString -> AST.Module
emptyModule label = AST.defaultModule {AST.moduleName = label}

initErrorModule:: AST.Module
initErrorModule = emptyModule "errorModule"

initModule :: AST.Module
initModule = emptyModule "koakModule"

-- process

process :: AST.Module -> String -> IO AST.Module
process _ "" = putStrLn "\ESC[91mERROR: There was a problem while reading source\ESC[0m"
              >>  return initErrorModule 
process _ source = do
  let res = runParser createAST source
  case res of
      Nothing -> putStrLn "\ESC[91mERROR : Syntax Error\ESC[0m" >> return initErrorModule
      Just (Error s, _) -> putStr "\ESC[91mERROR : " >> putStrLn (s ++ "\ESC[0m")
                                               >> return initErrorModule
      Just (ast, _) -> astToLLVM ast >> return initModule


processFiles :: [String] -> IO AST.Module
processFiles fnames = concatSources "" fnames >>= process initModule

concatSources :: String -> [String] -> IO String
concatSources base [] = return base
concatSources base (x:xs) = do
    src <- try $ readFile x
    case src of
        Left except -> putStr"\ESC[91m" >> print(except :: IOError )
                         >> putStr"\ESC[0m" >> return ""
        Right s -> concatSources (base ++ cleanSource) xs
                    where cleanSource = replace '\n' ' ' s


replace :: Eq b => b -> b -> [b] -> [b]
replace a b = map $ Data.Maybe.fromMaybe b . mfilter (/= a) . Just

repl :: IO ()
repl = putStrLn "\ESC[96mKOAK Version 1.0.0\nCopyright 2021-2022 Epitech Roazhon, Inc.\ESC[0m"
            >> runInputT defaultSettings (loop initModule)
    where
        loop mod = do
            maybeInput <- getInputLine "koak> "
            case maybeInput of
                Nothing -> outputStrLn "Goodbye."
                Just input -> do
                    newModule <- liftIO $ process mod input
                    loop newModule