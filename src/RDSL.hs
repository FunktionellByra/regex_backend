module RDSL
    -- (
    --   -- * Datatype for Regex tokens
    --   Regex(..)
    --   -- * @Regex@ pretty print
    -- , pp
    -- , eps
    -- , str
    -- , clazz
    -- )
    
    where

import Data.Maybe (isJust)
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

-- | Primitive Operations
eps,dot :: Regex
eps = Literal '\0'
dot = Dot

lit :: Char -> Regex
lit = Literal

oneOrMore,zeroOrMore,optional :: Regex -> Regex
zeroOrMore = Kleene
oneOrMore  = Plus
optional   = Optional

-- | Combinators

(+++) :: Regex -> Regex -> Regex
(+++) = Concat

(<|>) :: Regex -> Regex -> Regex
(<|>) = Or

clazz :: [Char] -> Regex
clazz = Class

--- | Derived Operations

repl :: Int -> Regex -> Regex
repl n r | n <= 0    = eps
         | otherwise = Concat r $ repl (n-1) r

str :: String -> Regex
-- We choose the tail-recursive function as we do not need laziness (also, regex are finite!)
str = foldl (\acc c -> acc +++ lit c) eps

alphaNum :: Regex
alphaNum = chr <|> digit

word :: Regex
word = oneOrMore alphaNum

digit :: Regex
digit = clazz ['0'..'9']

chr :: Regex
chr = clazz $ ['a'..'z'] ++ ['A'..'Z']

specialChrs :: Regex
specialChrs =
    clazz [
          '.'
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

symbol :: Regex
symbol = alphaNum <|> specialChrs

domain :: Regex
domain = oneOrMore chr

{-
a URL with protocol
an ISO date
a semantic version
a currency amount
a markdown link

a quoted string (handling escapes correctly)
-}
email :: Regex
email = oneOrMore symbol +++
        lit '@'          +++
        word             +++
        lit '.'          +++
        domain  

protocol :: Regex 
protocol = foldr1 Or (map str protocols)
    where protocols = ["http","https","file","ftp","udp"]

url :: Regex
url = protocol +++ str "://" +++
    (oneOrMore (word +++ lit '.') +++ domain +++
        oneOrMore (word +++ optional  (lit '/')))

email_with_special_chars :: String -> Regex
email_with_special_chars = undefined