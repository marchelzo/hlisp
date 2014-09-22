module Main where

import Parser
import Lisp
import LispMath()
import Control.Monad.State
import Control.Monad

main :: IO ()
main = execStateT (runREPL replSession) defaultContext >> return ()

replSession :: REPL ()
replSession = do
    liftIO $ putStr "> "
    exprStr <- liftIO getLine
    let expr = readExpr exprStr
    result <- eval expr
    liftIO $ print result
    replSession
