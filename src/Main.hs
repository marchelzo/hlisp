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
    when (exprStr /= "") $ do
        let expr = readExpr exprStr
        case expr of
            Right e -> do
                result <- eval e
                liftIO $ print result
            Left er -> liftIO (putStrLn ("syntax error:\n" ++ show er))
    replSession
