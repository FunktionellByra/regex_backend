module Parser
    ( Regex(..)
    , parseRegex
    , eps
    ) where

import Data.Functor.Identity
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec
import Prelude hiding (concat)
import Data.Char (isAlphaNum)

data Regex
    = Dot
    | Literal  Char
    | Plus     Regex
    | Kleene   Regex
    | Optional Regex
    | Concat Regex Regex
    | Or     Regex Regex
    | Class  [Char]
    deriving (Eq,Show,Read)

eps :: Regex
eps = Literal '\0' -- 'empty char' (sort-of)

parseRegex :: String -> Either ParseError Regex
parseRegex ""    = return eps
parseRegex input = parse regexParser "" input 

regexParser :: Parser Regex
regexParser = do
    rs <- many1 regexParserWithoutConcat
    -- eof -- require the whole input to be consumed TODO
    return $ foldl1 Concat rs

regexParserWithoutConcat :: Parser Regex
regexParserWithoutConcat = buildExpressionParser table atom

atom :: Parser Regex
atom = dotParser <|> literalParser <|> classParser <|> parenAtom
    where parenAtom = do 
            char '('
            r <- regexParser
            char ')'
            return r

dotParser :: Parser Regex
dotParser = Literal <$> char '.'

literalParser :: Parser Regex
literalParser = Literal <$> alphaNum

classParser :: Parser Regex
classParser = do
    char '['
    ls <- many alphaNum
    char ']'
    return $ Class ls

table :: OperatorTable String () Identity Regex
table =
  [ [ Postfix (char '*' >> return Kleene)
    , Postfix (char '+' >> return Plus)
    , Postfix (char '?' >> return Optional) ]
    , [Infix (char '|' >> return Or) AssocLeft ] ]

-- * Utils

pp :: Regex -> String
pp Dot            = "."
pp (Literal '\0') = ""
pp (Literal c)    = [c]
pp (Plus r)       = "(" ++ pp r ++ ")+"
pp (Kleene r)     = "(" ++ pp r ++ ")*"
pp (Optional r)   = "(" ++ pp r ++ ")?"
pp (Concat a b)   = pp a ++ pp b
pp (Or a b)       = "(" ++ pp a ++ "|" ++ pp b ++ ")"
pp (Class c)      = "[" ++ c ++ "]"

specialChrs :: String
specialChrs =
    [ '.'
    , '*'
    , '+'
    , '*'
    , '?'
    , '('
    , ')'
    , '['
    , ']'
    , '\\'
    , '^'
    , '$' ]