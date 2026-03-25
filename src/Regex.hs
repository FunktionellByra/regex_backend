module Regex
    ( match           -- * AST regex on string input
    , match1          -- * (string) regex on string input
    , matchWithTrace  -- * (string) regex on string input with trace
    , checkWithTrace  -- * regex (DFA) on string input with trace (internal)
    ) where

import Datatypes
import qualified DFA (fromNFAMulti, flattenToDFA, fromNFA)
import qualified NFA (epsilonClosure, fromRegex)
import Text.ParserCombinators.Parsec (ParseError)

import qualified DMap
import qualified Parser as P
import qualified Data.Map as Map
import qualified Control.Monad.State as S

type RegPattern     = String
type MatchWithTrace = (Bool,[(String, State, Bool)])

-- Match a (stringified) Regex against a string
match1 :: RegPattern -> String -> Either ParseError Bool
match1 p s = case P.parseRegex p of
    Right reg -> Right $ match reg s
    Left e -> Left e

-- Match a (stringified) Regex against a string input with trace
matchWithTrace :: RegPattern -> String -> Either ParseError MatchWithTrace
matchWithTrace p s = case P.parseRegex p of
    Right reg -> Right $ match2 reg s
    Left e -> Left e

-- Match a Regex datatype against a string input
match :: P.Regex -> String -> Bool
match pattern input = let dfa = (DFA.fromNFA . NFA.fromRegex) pattern in 
    check dfa input

match2 :: P.Regex -> String -> MatchWithTrace
match2 pattern input = let dfa = (DFA.fromNFA . NFA.fromRegex) pattern in 
    checkWithTrace dfa input

-- Match a Regex (as a DFA construct) against a string input
-- Note: useful for internal testing; @match@ calls @check@.
check :: DFA -> String -> Bool
check dfa@(DFA start accepts ts) = go start
    where
        go :: State -> String -> Bool
        go current []     = current `elem` accepts
        go current (c:cs) = any proceedWithChar ['.', c] -- lazy
            where 
                proceedWithChar :: Char -> Bool
                proceedWithChar c = case Map.lookup c $ DMap.lookup current ts of
                    Nothing        -> False
                    Just nextState -> go nextState cs

-- Works the same as @check@, but at each step of the traversal marks the state.
checkWithTrace :: DFA -> String -> MatchWithTrace
checkWithTrace dfa@(DFA start accepts ts) input =
    let (matched, state) = S.runState (go start input) (False, []) in
        (matched, reverse $ snd state)
    where
        go :: State -> String -> S.State MatchWithTrace Bool
        go current []     = do
            let isAccepting = current `elem` accepts
            -- The current state verifies the input, thus set the FoundFlag to True
            -- to avoid adding further trace
            S.modify (\(_, trace) -> if isAccepting then (True,([], current,True):trace) else (False,([], current,False):trace))
            return isAccepting
        go current inp@(c:cs) = do
            let literalsForCurrent = Map.keys $ DMap.lookup current ts
            -- If the next state has a dot transition, consider both that path
            -- and the literal path
            matched <- mapM (proceedWithChar inp) $
                if '.' `elem` literalsForCurrent then ['.', c] else [c]
            return $ or matched
            where 
                proceedWithChar :: String -> Char -> S.State MatchWithTrace Bool
                proceedWithChar inp c = case Map.lookup c $ DMap.lookup current ts of
                    Nothing        -> do
                        -- Add the current state as 'faulty', retain the found flag
                        S.modify (\s@(found, trace) -> if found then s else (False,(inp, current,False):trace))
                        return False
                    Just nextState -> do
                        -- Add the current state as valid on the verification path, retain the found flag
                        S.modify (\s@(found, trace) -> if found then s else (False,(inp, current,True):trace))
                        go nextState cs
