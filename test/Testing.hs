-- | A stand-alone testing module for Regex parser. 

module Main where

import RDSL            (Regex(..),eps)
import Regex           (match, match1)
import Data.List       (nub,sort)
import Text.Regex.TDFA ((=~))
import Debug.Trace     (trace)

import Test.QuickCheck

main :: IO ()
main = do
  quickCheck prop_rgx_positive
  quickCheck prop_rgx_negativish

  -- quickCheck prop_neutral_element_negative
  -- quickCheck prop_neutral_element_positive
{-
Idea:

1. Generate a random AST of our Tokens
  Positive Testing: 
    - From the AST, generate a String that definitely matches this AST
  Naive testing (will result mostly in negative testing):
    - From this AST, construct a Haskell-style regular expression and see if our algorithm and
      the Haskell-implementation result in the same 'match'-output
    - generate a random String and run both on it
-}

genAST :: Gen Regex
genAST = frequency [(24, sized helper), (1,return eps)]
  where 
    helper :: Int -> Gen Regex
    helper 0 = genLiteral
    helper n = frequency [
              (1, genOr n)
            , (1, genConcat n)
            , (2, genLiteral)
            , (1, genPlus n)
            , (1, genKleene n)
            , (1, genOptional n)
            , (1, genClass)
          ]

    genLiteral :: Gen Regex
    genLiteral = Literal <$> allowedLits

    genPlus :: Int -> Gen Regex
    genPlus k = let k' = k - 1 in do
      a <- helper k'
      return $ Plus a

    genKleene :: Int -> Gen Regex
    genKleene k = let k' = k - 1 in do
      a <- helper k'
      return $ Kleene a

    genOptional :: Int -> Gen Regex
    genOptional k = let k' = k - 1 in do
      a <- helper k'
      return $ Optional a

    genConcat :: Int -> Gen Regex
    genConcat k =  let k' = k `div` 2 in do
      a <- helper k'
      b <- helper k'
      return (Concat a b)

    genOr :: Int -> Gen Regex
    genOr k =  let k' = k `div` 2 in do
      a <- helper k'
      b <- helper k'
      return (Or a b)

    genClass :: Gen Regex
    genClass = do 
      rs <- sort . nub . take 5 <$> listOf allowedLits  
      return $ Class rs
    
    allowedLits = elements $ ['a' .. 'z'] ++ ['A'..'Z'] ++ ['0'..'9']

instance Arbitrary Regex where
  arbitrary = genAST

createRgx r = "^" ++ createRegex r ++ "$"

createRegex :: Regex -> String
createRegex Dot            = "\\."
createRegex (Literal '\0') = "()"
createRegex (Literal l)    = escape l
  where
    escape :: Char -> String
    escape c     = if c `elem` specialChars then "\\" ++ [c] else [c]
    specialChars = [
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
      , '$'
      ]
createRegex (Plus r)     = "(" ++ createRegex r ++ ")+"
createRegex (Kleene r)   = "(" ++ createRegex r ++ ")*"
createRegex (Optional r) = "(" ++ createRegex r ++ ")?"
createRegex (Concat a b) = createRegex a ++ createRegex b
createRegex (Or a b)     = "(" ++ createRegex a ++ "|" ++ createRegex b ++ ")"
createRegex (Class [])   = "[" ++ createRegex eps ++ "]"
createRegex (Class xs)   = "[" ++ xs ++ "]"

-- TODO: use QuickCheck to randomly populate constructors instead of using
-- minimal implementations.
createMatch :: Regex -> String
-- createMatch Epsilon      = ""
createMatch Dot          = "a"
createMatch (Literal c)  = [c]
createMatch (Plus r)     = createMatch r -- minimal r+
createMatch (Kleene r)   = ""            -- minimal r*
createMatch (Optional r) = ""            -- minimal r?
createMatch (Concat a b) = createMatch a ++ createMatch b
createMatch (Or a _)     = createMatch a -- bias on `a`
createMatch (Class [])   = ""
createMatch (Class xs)   = (createMatch . Literal . head) xs

prop_rgx_positive :: Regex -> Bool
prop_rgx_positive r = let 
  haskellRegex = createRgx r
  perfectMatch = createMatch r
  in match1 r perfectMatch == fullMatch perfectMatch haskellRegex

-- TODO: create more meaningful input strings => the input is meaningful even if its not human-readable
prop_rgx_negativish :: Regex -> String -> Bool
prop_rgx_negativish r input = let 
  haskellRegex = createRgx r
  in match1 r input == fullMatch input haskellRegex

prop_neutral_element_positive :: Regex ->  Bool
prop_neutral_element_positive r = let 
  perfectMatch = createMatch r
  in match1 r perfectMatch == match1 (Concat eps r) perfectMatch && 
     match1 r perfectMatch == match1 (Concat r eps) perfectMatch

prop_neutral_element_negative :: Regex -> String -> Bool
prop_neutral_element_negative r input = let 
  in match1 r input == match1 (Concat eps r) input && 
     match1 r input == match1 (Concat r eps) input

fullMatch :: String -> String -> Bool
fullMatch input pattern =
    case input =~ pattern :: (Int, Int) of
        (0, len) -> len == length input
        _        -> False
