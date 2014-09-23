module LispMath where

import Data.List (intersperse)

data Expr = Number Double
          | String String
          | Symbol String
          | Define String Expr
          | Cons Expr [Expr]
          | Nil
          | List [Expr]
          | Bool Bool
          | If Expr Expr Expr
          | Bottom
          | Lambda [String] Expr
          | Fn ([Expr] -> Expr)

instance Show Expr where
    show (Fn _)       = "<procedure>"
    show (Lambda _ _) = "<procedure>"
    show (Number x)   = show x
    show (String s)   = show s
    show (List xs)    = show xs
    show (Symbol s)   = "symbol `" ++ show s ++ "`"
    show Bottom       = ""
    show (Define _ _) = "<define>"
    show (Cons x xs)  = "(" ++ concat (intersperse " " $ (map show (x:xs))) ++ ")"
    show Nil          = "Nil"
    show (Bool True)  = "#t"
    show (Bool False) = "#f"
    show _            = "<procedure>"


plus :: [Expr] -> Expr
plus = Number . sum . map (\(Number x) -> x)

minus :: [Expr] -> Expr
minus [Number x, Number y] = Number (x - y)

mult :: [Expr] -> Expr
mult = Number . product . map (\(Number x) -> x)

divide :: [Expr] -> Expr
divide [Number x, Number y] = Number (x / y)

cons :: [Expr] -> Expr
cons [x, Cons y ys] = Cons x (y:ys)
cons [x, Nil]       = Cons x []

car :: [Expr] -> Expr
car [Cons x _] = x

cdr :: [Expr] -> Expr
cdr [Cons _ (x:xs)] = Cons x xs
cdr [Cons _ []]       = Nil

nil :: [Expr] -> Expr
nil [Nil] = Bool True
nil _     = Bool False

lispIf :: [Expr] -> Expr
lispIf [(Bool False), _, x] = x
lispIf [_, x, _]            = x

eq :: [Expr] -> Expr
eq [Number x, Number y] = Bool (x == y)
eq [String x, String y] = Bool (x == y)
eq [_, _]               = Bool False

mkList :: [Expr] -> Expr
mkList [] = Nil
mkList (x:xs) = Cons x xs
