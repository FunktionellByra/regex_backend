-- Parser module using @Parsec@.
module Parser
    (
      -- * ADST
      Regex(..)
      -- * Top-level utility to parse a @String@ to @Regex@
    , parseRegex
      -- * etc
    , eps
    , pp
    ) where

import Data.Functor.Identity
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec

import Datatypes      (specialChrs)
import Prelude hiding (concat)
import Data.Char      (isAlphaNum)

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
    r <- partialRegexParser
    eof 
    return r

partialRegexParser :: Parser Regex
partialRegexParser = do
    rs <- many1 regexParserWithoutConcat
    return $ foldl1 Concat rs

regexParserWithoutConcat :: Parser Regex
regexParserWithoutConcat = buildExpressionParser table atom

atom :: Parser Regex
atom = dotParser <|> literalParser  <|> classParser <|> parenAtom
    where parenAtom = do 
            char '('
            r <- partialRegexParser
            char ')'
            return r

dotParser :: Parser Regex
dotParser = Literal <$> char '.'

alphaNumParser :: Parser Regex
alphaNumParser = Literal <$> alphaNum

literalParser :: Parser Regex
literalParser = specialCharParser <|> alphaNumParser 

specialCharParser :: Parser Regex
specialCharParser = do
    char '\\'
    r <- oneOf specialChrs
    return $ Literal r

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