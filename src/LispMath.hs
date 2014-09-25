module LispMath where

import Data.List (intersperse)
import Text.Printf (printf)

data Expr = Number Double
          | String String
          | Symbol String
          | Define String Expr
          | Eval Expr
          | Quoted Expr
          | List [Expr]
          | Bool Bool
          | If Expr Expr Expr
          | Bottom
          | Lambda [String] Expr
          | Fn ([Expr] -> Expr)
          | Error String

instance Show Expr where
    show (Fn _)       = "<procedure>"
    show (Lambda _ _) = "<procedure>"
    show (Number x)   = showNum x
    show (String s)   = show s
    show (List xs)    = show xs
    show (Symbol s)   = s
    show Bottom       = ""
    show (Define _ _) = "<define>"
    show (Bool True)  = "#t"
    show (Bool False) = "#f"
    show (Error e)    = e
    show (Quoted q)   = showQuoted q
    show _            = "<procedure>"


plus :: [Expr] -> Expr
plus = Number . sum . map (\(Number x) -> x)

minus :: [Expr] -> Expr
minus [Number x, Number y] = Number (x - y)

mult :: [Expr] -> Expr
mult = Number . product . map (\(Number x) -> x)

divide :: [Expr] -> Expr
divide [Number x, Number y] = Number (x / y)

lispSqrt :: [Expr] -> Expr
lispSqrt [Number x] = Number (sqrt x)

lispExp :: [Expr] -> Expr
lispExp [Number x] = Number (exp x)

cons :: [Expr] -> Expr
cons [x, Quoted (List xs)] = Quoted (List (x:xs))

car :: [Expr] -> Expr
car [Quoted (List (x:_))] = x

cdr :: [Expr] -> Expr
cdr [Quoted (List (_:xs))] = Quoted (List xs)
cdr e                      = Error ("error: cdr takes a pair, given: " ++ show e)

nil :: [Expr] -> Expr
nil [(Quoted (List []))] = Bool True
nil _                    = Bool False

lispIf :: [Expr] -> Expr
lispIf [(Bool False), _, x] = x
lispIf [_, x, _]            = x

eq :: [Expr] -> Expr
eq [Number x, Number y] = Bool (x == y)
eq [String x, String y] = Bool (x == y)
eq [_, _]               = Bool False
eq _                    = Error "error: eq takes two arguments that"

mkList :: [Expr] -> Expr
mkList xs = Quoted (List xs)

lispAnd :: [Expr] -> Expr
lispAnd [Bool p, Bool q] = Bool (p && q)
lispAnd _                = Error "error: and requires two boolean arguments"

lispOr :: [Expr] -> Expr
lispOr [Bool p, Bool q] = Bool (p || q)
lispOr _                = Error "error: or requires two boolean arguments"

showNum :: Double -> String
showNum x
    | x == fromIntegral (round x) && ((not . elem 'e') (show x)) = takeWhile (/='.') (show x)
    | otherwise                   = printf "%-10f" x

showQuoted :: Expr -> String
showQuoted (List xs) = "(" ++ concat (intersperse " " $ (map show xs)) ++ ")"
showQuoted q         = show q
