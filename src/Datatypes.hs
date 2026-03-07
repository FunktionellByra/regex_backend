-- | Shared datatypes and definitions
module Datatypes
    ( State
    , MultiState
    , EpsClosure
    , NFA(..)
    , DFA(..)
    , PowerSetDFA(..)
    , NFATransitions
    , DFATransitions
    , PowerSetDFATransitions
    ) where

import Debug.Trace (trace)

import qualified DMap
import qualified Data.Map as Map
import DMap (DefaultMap)
import Data.Map (Map)

type State      = Int
type MultiState = [State]
type EpsClosure = DefaultMap State [State]

type NFATransitions = DefaultMap State (DefaultMap Char [State])
data NFA = NFA State State NFATransitions
instance Show NFA where
    show = stringifyNFA

type DFATransitions = DefaultMap State (Map Char State)
data DFA = DFA State [State] DFATransitions
instance Show DFA where
    show = stringifyDFA

type PowerSetDFATransitions = DefaultMap MultiState (Map Char MultiState)
data PowerSetDFA = PowerSetDFA MultiState [MultiState] PowerSetDFATransitions
instance Show PowerSetDFA where
    show = stringifyPowerSetDFA

-- * Stringify helpers for datatypes; not exposed in the module but override
-- @Show@.

-- | PowerSetDFA
stringifyPowerSetDFA :: PowerSetDFA -> String
stringifyPowerSetDFA (PowerSetDFA startMs endMss ts) =
    unlines [ "digraph G{"
           , headers
           , staticBody
           , body, "}" ]

    where
        headers = unlines
            [ indent "{"
            , unlines $ map (\state ->
                (indent . indent) $ "S" ++ formatMultiState state ++
                    "[shape=doublecircle]") endMss
            , indent "}"
            ]

        staticBody = unlines $ map indent
            [ "empty [label=\"\", shape=none,height=.0,width=.0]"
            , "empty -> S" ++ formatMultiState startMs ]

        body = transitionsToString ts
        transitionsToString :: PowerSetDFATransitions -> String
        transitionsToString ts = go (DMap.toList ts) ""
            where
                go :: [(MultiState, Map Char MultiState)] -> String -> String
                go [] accu       = accu
                go ((state, dMap):tss) accu =
                    go tss (accu ++ createEdges state (Map.toList dMap))

        createEdges :: MultiState -> [(Char, MultiState)] -> String 
        createEdges start xs = unlines [createEdge start ms ch | (ch, ms) <- xs]
       
        createEdge :: MultiState -> MultiState -> Char -> String
        createEdge s t l = "\tS" ++ formatMultiState s ++ " -> S" ++
                            formatMultiState t ++ " [label=" ++ formatChar l ++ "]"

        formatMultiState :: MultiState -> String
        formatMultiState = concatMap show

-- | DFA
stringifyDFA :: DFA -> String
stringifyDFA (DFA start ends ts) =
    unlines [ "digraph G{"
           , headers
           , "//COLORS" -- stud to be replaced by dynamic trace (client)
           , staticBody
           , body, "}" ]

    where
        headers = unlines
            [ indent "{"
            , unlines $ map(\state ->
                (indent . indent) $ "S" ++ show state ++ "[shape=doublecircle]"
                ) ends
            , indent "}"
            ]

        staticBody = unlines $ map indent
            [ "empty [label=\"\", shape=none,height=.0,width=.0]"
            , "empty -> S" ++ show start ]

        body = transitionsToString ts
        transitionsToString :: DFATransitions -> String
        transitionsToString ts = go (DMap.toList ts) ""
            where
                go :: [(State, Map Char State)] -> String -> String
                go [] accu       = accu
                go ((state, dMap):tss) accu = go tss (accu ++ inner state (Map.toList dMap))

        -- For a state, create edges to all other states via char literal transitions
        inner :: State -> [(Char, State)] -> String 
        inner start xs = unlines [createTransition start s ch | (ch, s) <- xs]

-- | NFA
stringifyNFA :: NFA -> String
stringifyNFA (NFA start end ts) =
    unlines [ "digraph G{"
            , headers
            , "//COLORS" -- stud to be replaced by dynamic trace (client)
            , staticBody
            , body, "}" ]

    where
        headers = unlines $ map indent
            [ "{"
            , indent $ "S" ++ show end ++ "[shape=doublecircle]"
            , "}" ]

        staticBody = unlines $ map indent
            [ "empty [label=\"\", shape=none,height=.0,width=.0]"
            , "empty -> S" ++ show start ]

        body = concat $ collectTransitionStrings ts

        -- Collect all of the transitions as strings in a list
        -- transitions are computed via `createTransition`
        collectTransitionStrings :: NFATransitions -> [String]
        collectTransitionStrings ts = go (DMap.toList ts) []
            where
                go :: [(State, DefaultMap Char [State])] -> [String] -> [String]
                go []                  accu = accu
                go ((state, dMap):tss) accu =
                    let newTransitions = createTransitions state (DMap.toList dMap) in
                        go tss (newTransitions:accu)

        -- Create edges from a (start) state to other states via multiple paths
        -- (i.e., char literals).
        createTransitions :: State -> [(Char, [State])] -> String 
        createTransitions start xs =
            concat [unlines
                [createTransition start s ch | s <- ss]
                    | (ch, ss) <- xs ]


-- | Shared helpers

formatChar :: Char -> String
formatChar '.' = "\8226"
formatChar ':' = "\":\""
formatChar '/' = "\"/\""
formatChar l   = [l]

indent :: String -> String
indent xs = if null xs then "" else "\t" ++ xs

createTransition :: State -> State -> Char -> String
createTransition s t l = "\tS" ++ show s ++ " -> S" ++ show t ++
                        " [label=" ++ formatChar l ++ "]"