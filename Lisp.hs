{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lisp where

import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Applicative

import LispMath

type Context = M.Map String Expr

defaultContext :: Context
defaultContext = M.fromList [("+", Fn plus), ("-", Fn minus), ("*", Fn mult), ("/", Fn divide)]

newtype REPL a = REPL {
    runREPL :: StateT Context IO a
} deriving (Monad, MonadIO, MonadState Context, Functor, Applicative)

eval :: Expr -> REPL Expr
eval (Number x) = return (Number x)
eval (String s) = return (String s)
eval (Symbol s) = do
    ctx <- get
    return (ctx M.! s)
eval (List (x:xs)) = do
    xs' <- sequence $ map eval xs
    x' <- eval x
    apply x' xs'
eval (Fn f) = return (Fn f)
eval (Lambda cs e) = return (Lambda cs e)
eval (Define s e) = do
    ctx <- get
    expr <- eval e
    let newCtx = M.insert s expr ctx
    put newCtx
    return Bottom

apply :: Expr -> [Expr] -> REPL Expr
apply (Fn f) xs = return $ f xs
apply (Lambda cs e) xs = do
    ctx <- get
    let lambdaContext = M.fromList $ zip cs xs
    let newCtx = M.union lambdaContext ctx
    put newCtx
    result <- eval e
    put ctx
    return result
