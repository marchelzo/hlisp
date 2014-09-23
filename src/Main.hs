module Main where

import Parser
import Lisp
import LispMath()
import Control.Monad.State
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    void $ execStateT (runREPL replSession) defaultContext

replSession :: REPL ()
replSession = do
    liftIO $ putStr "> "
    exprStr <- liftIO getLine
    let expr = readExpr exprStr
    result <- eval expr
    liftIO $ print result
    replSession
