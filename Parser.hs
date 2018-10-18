module Parser (parse) where -- only expose the top-level parsing function

import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import Combinators
import qualified Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem)

data AST = ASum T.Operator AST AST
         | AProd T.Operator AST AST
         | AAssign String AST
         | ANum Integer
         | AUnaryMinus AST
         | APow AST AST
         | AIdent String
         | AList [AST]
         | AListEssence [AST]
         | AComma Char
         | AParenth Char
         | AListConcat AST AST

parse :: String -> Maybe(Result [AST])
parse input = 
  case input of
    [] -> Nothing
    _  -> Just (mapResult getFirst (parse' input))

getFirst :: (a, b) -> a
getFirst (a, b) = a

parse' :: Parser [AST]
parse' =
  (empty |> return [])
  <|> (statementList >>= \s -> empty |> return s)
  <|> zero ("Syntax error: only a prefix of the input is parsed")

statementList :: Parser [AST]
statementList =
  (globalExpression >>= \g ->
    semicolon |>
    statementList >>= \r -> return (g : r))
  <|> globalExpression >>= \e -> return [e]

globalExpression :: Parser AST
globalExpression =
  (listTerm >>= \l ->
   concatOp |>
  listConcat >>= \r -> return (AListConcat l r))
  <|> expression
  <|> listTerm

listConcat :: Parser AST
listConcat =
  (listTerm >>= \l ->
   concatOp |>
   listConcat >>= \r -> return (AListConcat l r))
  <|> listTerm
  
listTerm :: Parser AST
listTerm =
  (lsqparen |>
   rsqparen |> return (AList []))
  <|>
  (lsqparen |>
   listEssence >>= \e ->
   rsqparen |> return (AList e))
  <|> identifier
  
listEssence :: Parser [AST]
listEssence =
  (globalExpression >>= \e  -> 
   comma |>
   listEssence >>= \t -> return (e : t)
   )
  <|> globalExpression >>= \e -> return (e : [])

expression :: Parser AST
expression =
  ( identifier >>= \(AIdent i) ->
    assignment |>
    globalExpression >>= \e -> return (AAssign i e)
  )
  <|> ( term       >>= \l  -> -- Here the identifier is parsed twice :(
        plusMinus  >>= \op ->
        expression >>= \r  -> return (ASum op l r)
      )
  <|> term

term :: Parser AST
term =
  -- make sure we don't reparse the factor (Term -> Factor (('/' | '*') Term | epsilon ))
  unaryTerm >>= \l ->
  ( ( divMult >>= \op ->
      term    >>= \r  -> return (AProd op l r)
    )
    <|> return l
  )

unaryTerm :: Parser AST
unaryTerm = 
  powTerm
  <|> (unaryMinus|> powTerm >>= \f -> return (AUnaryMinus f))

powTerm :: Parser AST
powTerm = (factor >>= \l -> 
           pow |>
           unaryTerm >>= \r -> return (APow l r))
          <|> factor
  
factor :: Parser AST
factor =
  ( lparen |>
    expression >>= \e ->
    rparen |> return e -- No need to keep the parentheses
  )
  <|> identifier
  <|> numeric
  

numeric :: Parser AST
numeric = map (ANum) (number)

identifier :: Parser AST
identifier = map (AIdent) (ident)


lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

lsqparen :: Parser Char
lsqparen = char '['

rsqparen :: Parser Char
rsqparen = char ']'

comma :: Parser Char
comma = char ','

semicolon :: Parser Char
semicolon = char ';'

assignment :: Parser Char
assignment = char '='

pow :: Parser Char
pow = char '^'

unaryMinus :: Parser Char
unaryMinus = char '-'

concatOp :: Parser Char
concatOp = char '+' |> char '+'

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

divMult :: Parser T.Operator
divMult   = map T.operator (char '/' <|> char '*')

deleteSpaces :: String -> String
deleteSpaces [] = []
deleteSpaces (c : cs) | isSpace c = cs'
                      | otherwise = (c : cs')
                      where cs' = deleteSpaces cs
   
                     

instance Show AST where
  show tree = "\n" ++ show' 0 0 tree
    where
      show' n f t =
        (if (n > 0) && (f == 1) then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) 1  l ++ "\n" ++ show' (ident n) 1 r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) 1 l ++ "\n" ++ show' (ident n) 1 r
                  AAssign  v e -> v ++ " =\n" ++ show' (ident n) 1 e
                  ANum   i     -> show i
                  AIdent i     -> show i
                  AUnaryMinus v -> "-\n" ++ show' (ident n) 1 v
                  APow l r -> '^' : "\n" ++ show' (ident n) 1 l ++ "\n" ++ show' (ident n) 1 r 
                  AListConcat l r -> "++\n" ++ show' (ident n) 1 l ++ "\n" ++ show' (ident n) 1 r
                  AList list -> "[" ++ show' (ident n) 0 (AListEssence list) ++ "\n" ++ show' n 1 (AParenth ']')
                  AListEssence (e : []) -> "\n" ++ show' (ident n) 1 e
                  AListEssence (e : es) -> "\n" ++ show' (ident n) 1 e ++ "\n" ++ show' n 1 (AComma ',') ++ show' n 0 (AListEssence es)
                  AListEssence [] -> ""
                  AComma c -> [c]
                  AParenth c -> [c])
      ident = (+1)
      showOp T.Plus  = '+'
      showOp T.Minus = '-'
      showOp T.Mult  = '*'
      showOp T.Div   = '/'