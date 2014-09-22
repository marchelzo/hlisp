module LispMath where

data Expr = Number Double
          | String String
          | Symbol String
          | Define String Expr
          | List [Expr]
          | Bottom
          | Lambda [String] Expr
          | Fn ([Expr] -> Expr)

instance Show Expr where
    show (Fn _) = "<procedure>"
    show (Lambda _ _) = "<procedure>"
    show (Number x)   = show x
    show (String s)   = show s
    show (List xs)    = show xs
    show (Symbol s)   = "symbol `" ++ show s ++ "`"
    show Bottom       = ""
    show (Define _ _) = "<define>"


plus :: [Expr] -> Expr
plus = Number . sum . map (\(Number x) -> x)

minus :: [Expr] -> Expr
minus [Number x, Number y] = Number (x - y)

mult :: [Expr] -> Expr
mult = Number . product . map (\(Number x) -> x)

divide :: [Expr] -> Expr
divide [Number x, Number y] = Number (x / y)
