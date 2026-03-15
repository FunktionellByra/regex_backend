module RDSL
    ( -- * AST for a regex
      Regex(..)

    -- * Primitive Operations
    , eps
    , dot
    , lit
    , oneOrMore 
    , zeroOrMore
    , optional
    , clazz 

    -- * Combinators
    , (+++)
    , (<|>)

    -- * Derived Operation
    , str
    , alphaNum
    , word
    , paddedWord
    , space
    , nat
    , digit

    -- ** Higher-order-regex
    , list
    , repl

    -- * Predefined classes
    , chr
    , specialChar
    , symbol
    , email
    , protocol
    , domain
    , url
    , isoDate
    , semanticVersion
    , markdownLink
    , markdownImage
    , csv

    -- * Utils
    , pp
    , specialChars
    ) where

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
pp (Class s)      = "[" ++ s ++ "]"

-- | Matches the empty string.
eps :: Regex
eps = Literal '\0'

-- | Matches any character.
dot :: Regex
dot = Dot

-- | Matches the character supplied.
lit :: Char -> Regex
lit = Literal

oneOrMore,zeroOrMore,optional :: Regex -> Regex
zeroOrMore = Kleene
oneOrMore  = Plus
optional   = Optional

clazz :: [Char] -> Regex
clazz = Class

(+++) :: Regex -> Regex -> Regex
(+++) = Concat

(<|>) :: Regex -> Regex -> Regex
(<|>) = Or

--- * Higher-order-Regex 

-- | Create a regular expression that represents @n@ times the regular expression @r@.
repl :: Int -> Regex -> Regex
repl n r | n <= 0    = eps
         | otherwise = Concat r $ repl (n-1) r

-- | Non-empty 'list' of @r@s delimited by @del@.
--   __Example__: a,b,c,d
list :: Regex -> Char -> Regex
list r del = zeroOrMore (r +++ lit del) +++ r

-- | Takes a string and creates a regex-representation of it.
str :: String -> Regex
-- We choose the tail-recursive function as we do not need laziness (also, regex are finite!)
str = foldl (\acc c -> acc +++ lit c) eps

-- | Matches an integral without leading zero.
nat :: Regex
nat = lit '0' <|> (digitNonZero +++ zeroOrMore digit)

-- | Matches exactly one alphanumeric character.
alphaNum :: Regex
alphaNum = chr <|> digit

-- | Matches a non-empty sequence of alphanumeric characters.
word :: Regex
word = oneOrMore alphaNum

-- | Matches a non-empty sequence of alphanumeric characters, possibly padded
--   with whitespaces on the left/right.
paddedWord :: Regex 
paddedWord =
    zeroOrMore space +++
    word             +++
    zeroOrMore space

-- | Matches an ASCII-whitespace
space :: Regex
space = lit ' '

--- * Predefined Classes

-- | Matches a digit
digit :: Regex
digit        = clazz ['0'..'9']

-- | Matches a non-zero digit
digitNonZero :: Regex
digitNonZero = clazz ['1'..'9']

-- | Matches an alphabetical character
chr :: Regex
chr = clazz $ ['a'..'z'] ++ ['A'..'Z']

-- | Matches a special character.
--  special characters include
-- >>> ['.' ,'*','+','*','?','(',')','[',']','\\','^','$']
specialChar :: Regex
specialChar = clazz specialChars

specialChars :: String 
specialChars =
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

-- | Matches a single alphanumeric or a special symbol from @specialChrs@.
symbol :: Regex
symbol = alphaNum <|> specialChar

domain :: Regex
domain = oneOrMore chr

email :: Regex
email = oneOrMore symbol +++
        lit '@'          +++
        word             +++
        lit '.'          +++
        domain  

-- | Matches any pre-specified protocl 
--  Currently supported: http, https, fi, ftp, udp, tcp
protocol :: Regex 
protocol = foldr1 Or (map str protocols)
    where protocols = ["http","https","file","ftp","udp","tcp"] -- examples

-- | Matches a unique resource locator with selected @protocol@s.
url :: Regex
url = protocol +++ str "://" +++
    (oneOrMore (word +++ lit '.') +++ domain +++
        oneOrMore (word +++ optional  (lit '/')))

-- | Matches an ISO date.
--   Format: `YYYY-MM-DD`, e.g., 2026-03-11
isoDate :: Regex
isoDate = repl 4 digit +++
          lit '-'      +++
          repl 2 digit +++
          lit '-'      +++
          repl 2 digit

-- | Matches a semantic versioning.
--   Format: `MAJOR.MINOR.PATCH`, e.g., 3.21.17
semanticVersion :: Regex
semanticVersion = nat +++ lit '.' +++
                  nat +++ lit '.' +++
                  nat

-- | Matches `MD` link.
--   Format: `[name](url)`
markdownLink :: Regex
markdownLink = lit '[' +++ paddedWord +++ lit ']' +++
               lit '(' +++ url        +++ lit ')'

-- | Matches `MD` image.
--   Format: `![name](url)`
markdownImage :: Regex
markdownImage = lit '!' +++ markdownLink

-- * Regex utils derived via higher-order-regex

-- | Unsafe function to parse a CSV. Will raise an error if delimiter not
--   supported (use ',', ';' or '\t'). This assumes that the last line is not
--   escaped.
--
-- __Example__:
--
-- @
-- let reg = csv ','  
--     input = "a,b,c\ne,f,g\nh,i,jfsdfds\n fd, d,,s"  
-- >>> True  
-- @
--
-- __Remark__:
-- Regular expression cannot reliably parse .csv-formats.  Thus, this function
-- can detect a subset of correct csv, but not reliably falsify wrong formats
-- (i.e. non-matching columns).  
csv :: Char -> Regex
csv del | del `notElem` allowedDels =
            error $ "csv: unsupported delimiter, use" ++ show allowedDels
        | otherwise                 = list row '\n'
    where
        allowedDels       = [',',';','\t']
        row               = list possiblyEmptyWord del
        possiblyEmptyWord = paddedWord <|> zeroOrMore space
