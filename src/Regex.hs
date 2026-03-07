module Regex
    ( match           -- * regex (string) on string input
    , match1          -- * regex (datatype) on string input
    , check           -- * internal
    , checkWithTrace  -- * internal (trace for visuals)
    , RegPattern      -- * alias for Stringified reg. patterns
    ) where

import Datatypes
import qualified DFA (fromNFAMulti, flattenToDFA, fromNFA)
import qualified NFA (epsilonClosure, fromRegex)
import Debug.Trace (trace)

import qualified DMap
import qualified RDSL as P -- TODO
import qualified Data.Map as Map
import qualified Control.Monad.State as S

type RegPattern = String

-- Match a (stringified) Regex against a string input
match :: RegPattern -> String -> Bool
match = undefined
-- match p s = case P.parseReg p of
--     Just reg -> match1 reg s
--     _        -> False

-- Match a Regex datatype against a string input
match1 :: P.Regex -> String -> Bool
match1 pattern input = let dfa = (DFA.fromNFA . NFA.fromRegex) pattern in 
    check dfa input

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

type TraversalTrace =
    ( Bool             -- * flag when in accepting state
    , [(State, Bool)]) -- * labeling (faulty/valid) for each state

-- Works the same as @check@, but at each step of the traversal marks the state.
-- This is useful for visualising the traversal step-by-step.
checkWithTrace :: DFA -> String -> (Bool, [(State, Bool)]) 
checkWithTrace dfa@(DFA start accepts ts) input =
    let (matched, state) = S.runState (go start input) (False, []) in
        (matched, reverse $ snd state)
    where
        go :: State -> String -> S.State TraversalTrace Bool
        go current []     = do
            let isAccepting = current `elem` accepts
            -- The current state verifies the input, thus set the FoundFlag to True
            -- to avoid adding further trace
            S.modify (\(_, trace) -> if isAccepting then (True,(current,True):trace) else (False,(current,False):trace))
            trace ("Accept states: " ++ show accepts) (return ())
            return isAccepting
        go current (c:cs) = do
            let literalsForCurrent = Map.keys $ DMap.lookup current ts
            -- If the next state has a dot transition, consider both that path
            -- and the literal path
            matched <- mapM proceedWithChar $
                if '.' `elem` literalsForCurrent then ['.', c] else [c]
            return $ or matched
            where 
                proceedWithChar :: Char -> S.State TraversalTrace Bool
                proceedWithChar c = case Map.lookup c $ DMap.lookup current ts of
                    Nothing        -> do
                        -- Add the current state as 'faulty', retain the found flag
                        S.modify (\s@(found, trace) -> if found then s else (False,(current,False):trace))
                        trace ("no transition found, false") (return ())
                        return False
                    Just nextState -> do
                        -- Add the current state as valid on the verification path, retain the found flag
                        S.modify (\s@(found, trace) -> if found then s else (False,(current,True):trace))
                        go nextState cs
