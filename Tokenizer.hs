module Tokenizer where

data Token = TNumber Integer
           | TIdent String
           | TOp Operator
           | TLParen
           | TRParen
           | TAssign
           | TEof
           deriving (Show, Eq)

data Operator = Plus
              | Minus
              | Mult
              | Div
              | Pow
              | UnaryMinus
              deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [TEof]
tokenize (c : cs) | isOperator c   = TOp (operator c) : tokenize cs
                  | isDigit c      = TNumber (read numStr :: Integer) : tokenize (drop ((length numStr) - 1) cs) 
                  | c == '('       = TLParen : tokenize cs
                  | c == ')'       = TRParen : tokenize cs
                  | c == '='       = TAssign : tokenize cs
                  | isWhiteSpace c = tokenize cs
                  | isCharacter c  = TIdent (ident) : tokenize (drop ((length ident) - 1) cs)
                  | otherwise = error ("Lexical error: unacceptable character " ++ [c])
                  where ident  = c : (getIdent cs)
                        numStr = c : (getNumStr cs)

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
           | c == '^' = Pow
operator c = error ("Lexical error: " ++ c : " is not an operator!")

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789"

digit :: Char -> Integer
digit c | c == '0' = 0
        | c == '1' = 1
        | c == '2' = 2
        | c == '3' = 3
        | c == '4' = 4
        | c == '5' = 5
        | c == '6' = 6
        | c == '7' = 7
        | c == '8' = 8
        | c == '9' = 9
digit c = error ("Lexical error: " ++ c : " is not a digit!")

isCharacter :: Char -> Bool
isCharacter c = c `elem` ['a' .. 'z']

getIdent :: String -> String
getIdent [] = []
getIdent (c : cs) | isDigit c || isCharacter c = c : getIdent cs
                  | otherwise = ""
                  
getNumStr :: String -> String
getNumStr [] = []
getNumStr (c : cs) | isDigit c = c : getNumStr cs
                   | otherwise = ""

alpha :: Char -> Char
alpha c = c

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"
