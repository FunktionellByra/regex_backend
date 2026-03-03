module Parser
    ( Regex(..) -- * Datatype for Regex tokens
    , pp        -- * @Regex@ pretty print
    , parseReg
    , parseReg1
    ) where

import Parsing

import Data.Maybe (isJust)
import Prelude hiding (concat)
import Data.Char (isAlphaNum)

data Regex
    = Epsilon
    | Dot
    | Literal  Char
    | Plus     Regex
    | Kleene   Regex
    | Optional Regex
    | Concat Regex Regex
    | Or     Regex Regex
    deriving (Eq, Show)

pp :: Regex -> String 
pp Epsilon                = "ε"
pp Dot                    = "."
pp (Literal l)            = [l]
pp (Kleene l@(Literal _)) = pp l ++ "*"
pp (Kleene d@Dot)         = pp d ++ "*"
pp (Kleene r)             = "(" ++ pp r ++ ")*"
pp (Concat r1 r2)         = pp r1 ++ pp r2

-- @parseReg@ only returns @Just reg@ if the sequence is parsed fully.
-- This simplifies testing.
parseReg :: String -> Maybe Regex
parseReg s = case parseReg1 s of
    Just (reg,"") -> Just reg
    _             -> Nothing

-- @parseReg1@ returns a subset of sucessfully parsed regex and the trailing
-- sequence.
parseReg1 :: String -> Maybe (Regex,String)
parseReg1 "" = Just (Epsilon,"")
parseReg1 s  = parse reg s

reg :: Parser Regex
reg  = concat
    <|> kleene
    <|> plus
    <|> dot
    <|> literal
    <|> eps

regWithOutConcat :: Parser Regex
regWithOutConcat = kleene 
                <|> plus
                <|> dot
                <|> literal
                <|> eps

literal :: Parser Regex
literal = literal1 <|> literal2

literal1 :: Parser Regex
literal1 = do 
    c <- sat isAlphaNum
    return $ Literal c

literal2 :: Parser Regex
literal2 = do
    char '('
    c <- literal1 <|> literal2 
    char ')'
    return c

eps :: Parser Regex
eps = do
    char '('
    char ')'
    return Epsilon

dot :: Parser Regex
dot = dot1 <|> dot2

dot1 :: Parser Regex
dot1 = do
    char '.'
    return Dot

dot2 :: Parser Regex
dot2 = do 
    char '('
    c <- dot1 <|> dot2
    char ')'
    return Dot

kleene :: Parser Regex
kleene = kleene1 <|> kleene2

kleene1 :: Parser Regex
kleene1 = do
    char '('
    r <- reg 
    char ')'
    char '*'
    return $ Kleene r

kleene2 :: Parser Regex
kleene2 = do
    r <- dot <|> literal <|> eps
    char '*'
    return $ Kleene r

concat :: Parser Regex
concat = concat1 <|> concat2

concat1 :: Parser Regex
concat1 = do
    head_r  <- regWithOutConcat
    tail_rs <- zeroOrMore reg
    return $ foldl Concat head_r tail_rs

concat2 :: Parser Regex
concat2 = do
    char '('
    c <- concat1 <|> concat2
    char ')'
    
    tail_rs <- zeroOrMore reg 
    return $ foldl Concat c tail_rs

plus :: Parser Regex 
plus = plus1 <|> plus2

plus1 :: Parser Regex
plus1 = do
    char '('
    r <- reg
    char ')'
    char '+'
    return $ Concat r (Kleene r) 

plus2 :: Parser Regex
plus2 = do
    r <- dot <|> literal <|> eps
    char '+'
    return $ Concat r (Kleene r)