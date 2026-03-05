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
    deriving (Eq,Show)

pp :: Regex -> String
pp Dot            = "."
pp (Literal '\0') = ""
pp (Literal c)    = [c]
pp (Plus r)       = "(" ++ pp r ++ ")+"
pp (Kleene r)     = "(" ++ pp r ++ ")*"
pp (Optional r)   = "(" ++ pp r ++ ")?"
pp (Concat a b)   = pp a ++ pp b
pp (Or a b)       = "(" ++ pp a ++ "|" ++ pp b ++ ")"

-- | Primitive Operations
eps,dot :: Regex
eps = Literal '\0'
dot = Dot

symbol :: Char -> Regex
symbol = Literal

plus,kleene,optional :: Regex -> Regex
plus     = Plus
kleene   = Kleene
optional = Optional

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
str = foldl (\acc c -> acc +++ symbol c) eps

alphaNum :: Regex
alphaNum = chr <|> digit

digit :: Regex
digit = clazz ['0'..'9']

chr :: Regex
chr = clazz $ ['a'..'z'] ++ ['A'..'Z']