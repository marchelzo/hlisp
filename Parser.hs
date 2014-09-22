module Parser where

import Text.Parsec
import Text.Parsec.String

import Lisp
import LispMath

readExpr :: String -> Expr
readExpr expr = case parse parseExpr "" expr of
    Left err   -> error $ show err
    Right expr -> expr

parseExpr :: Parser Expr
parseExpr = (try parseLambda) <|> (try parseDefine) <|> parseNumber <|> parseString <|> parseSymbol <|> parseList

parseNumber :: Parser Expr
parseNumber = do
              numStr <- many1 digit
              return $ Number (read numStr)

parseSymbol :: Parser Expr
parseSymbol = do
              sym <- many1 letter <|> (fmap (:[]) (oneOf "+*/-"))
              return $ Symbol sym

parseString :: Parser Expr
parseString = do
              _ <- char '"'
              s <- many $ noneOf "\""
              _ <- char '"'
              return $ String s

parseList :: Parser Expr
parseList = do
            _ <- char '('
            list <- sepBy parseExpr spaces
            _ <- char ')'
            return $ List list

parseDefine :: Parser Expr
parseDefine = do
    _ <- char '('
    _ <- string "def"
    _ <- char ' '
    s <- many1 letter
    _ <- char ' '
    e <- parseExpr
    _ <- char ')'
    return $ Define s e

parseLambda = do
    _        <- char '('
    _        <- string "lambda"
    _        <- char ' '
    _        <- char '('
    captured <- sepBy (many1 letter) (char ' ')
    _        <- char ')'
    _        <- char ' '
    expr     <- parseExpr
    _        <- char ')'
    return $ Lambda captured expr
