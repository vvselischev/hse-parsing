module Combinators where
-- Make sure that the names don't clash
import Prelude hiding (lookup, (>>=), map, pred, return, elem)

import Data.Char(isSpace, isDigit, isAlpha)

import Data.List(dropWhileEnd)

-- Input abstraction
type Input = String

-- Result is polymorphic in the ... result
data Result r = Success r
              | Error String
              deriving (Show)

-- The result of parsing is some payload r and the suffix which wasn't parsed
type Parser r = Input -> Result (r, Input)

-- Choice combinator: checks if the input can be parsed with either the first, or the second parser
-- Left biased: make sure, that the first parser consumes more input
infixl 6 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \inp ->
  case p inp of
    Error _ -> q inp
    result  -> result

-- Sequential combinator: if the first parser successfully parses some prefix, the second is run on the suffix
-- The second parser is supposed to use the result of the first parser
infixl 7 >>=
(>>=) :: Parser a -> (a -> Parser b ) -> Parser b
p >>= q = \inp ->
  case p (dropWhile isSpace inp) of
    Success (r, inp') -> q r (dropWhile isSpace inp')
    Error err -> Error err
    
-- Sequential combinator which ignores the result of the first parser
infixl 7 |>
(|>) :: Parser a -> Parser b -> Parser b
p |> q = p >>= const q


-- Succeedes without consuming any input, returning a value
return :: a -> Parser a
return r inp = Success (r, inp)

-- Always fails
zero :: String -> Parser a
zero err = const $ Error err

-- Chops of the first element of the string, ignores whitespaces
elem :: Parser Char
--elem (c : cs) | (isSpace c) = elem cs
--              | otherwise = Success (c, cs) 
elem (c : cs) = Success (c, cs) 
elem [] = Error "Empty string"

empty :: Parser Char
empty [] = Success (' ', [])
empty _  = Error "Unexpected symbol"

number :: Parser Integer
number cs = if length(numStr) > 0
            then Success ((read numStr ::Integer), drop ((length numStr)) cs)
            else Error "Number expected"
            where numStr = getNumStr cs
            
ident :: Parser String
ident cs = if length(identStr) > 0
           then Success (identStr, drop (length(identStr )) cs)
           else Error "Identifier expected"
           where identStr = getIdentStr cs

getIdentStr :: String -> String
getIdentStr [] = []
getIdentStr (c : cs) | isAlpha c = c : getIdentStr' cs
                     | otherwise = ""
                     
getIdentStr' :: String -> String
getIdentStr' [] = []
getIdentStr' (c : cs) | (isAlpha c) || (isDigit c) = c : getIdentStr' cs
                      | otherwise = ""
                
getNumStr :: String -> String
getNumStr [] = []
getNumStr (c : cs) | isDigit c = c : getNumStr cs
                   | otherwise = ""
                
-- Checks if the first character of the string is the given one
char :: Char -> Parser Char
char c = sat (== c) elem

-- Checks if the parser result satisfies the predicate
sat :: (a -> Bool) -> Parser a -> Parser a
sat pred parser inp =
  case parser inp of
    Success (r, inp') | pred r ->  Success (r, inp')
    Success _ -> Error "Predicate is not satisfied"
    Error err -> Error err

-- Applies the function to the result of the parser
map :: (a -> b) -> Parser a -> Parser b
map f parser inp =
  case parser inp of
    Success (r, inp') -> Success (f r, inp')
    Error err -> Error err