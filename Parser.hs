module Parser where

import Text.Parsec
import Text.Parsec.String
import Data.Char (isLower)

import Lisp
import LispMath

readExpr :: String -> Expr
readExpr expr = case parse parseExpr "" expr of
    Left err   -> error $ show err
    Right expr -> expr

parseExpr :: Parser Expr
parseExpr =     (try parseFunctionDef)
            <|> (try parseLambda)
            <|> (try parseDefine)
            <|> parseNil
            <|> parseNumber
            <|> parseString
            <|> parseSymbol 
            <|> parseList
            <|> parseBool

parseNumber :: Parser Expr
parseNumber = do
              numStr <- many1 digit
              return $ Number (read numStr)

parseSymbol :: Parser Expr
parseSymbol = do
              sym <- parseName <|> (fmap (:[]) (oneOf "+*/-"))
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
    _   <- char '('
    _   <- string "def"
    _   <- char ' '
    sym <- parseName
    _   <- char ' '
    e   <- parseExpr
    _   <- char ')'
    return $ Define sym e

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

parseFunctionDef :: Parser Expr
parseFunctionDef = do
    _        <- string "(def ("
    fn       <- parseName
    _        <- char ' '
    captured <- sepBy (many1 letter) (char ' ')
    _        <- string ") "
    expr     <- parseExpr
    _        <- char ')'
    return $ sugaredLambda fn captured expr
    where
        sugaredLambda f cap e = Define f (Lambda cap e)

parseNil :: Parser Expr
parseNil = do
    _ <- string "Nil"
    return Nil

parseBool :: Parser Expr
parseBool = do
    _ <- char '#'
    b <- char 't' <|> char 'f'
    return $ case b of
        't' -> Bool True
        'f' -> Bool False

parseName :: Parser String
parseName = do
    fst  <- satisfy isLower
    name <- many $ (satisfy isLower) <|> char '-'
    qm   <- optionMaybe (char '?')
    return $ case qm of
                Just _  -> (fst:name) ++ "?"
                Nothing -> (fst:name)
