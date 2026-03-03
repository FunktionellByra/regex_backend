module NFA
    ( fromRegex
    , epsilonClosure
    ) where

import qualified DMap
import qualified Data.Map as Map
import DMap (DefaultMap)
import Data.Map (Map)

import Datatypes
import Parser (Regex(..))

import qualified Control.Monad.State as S

data Env = Env
    { transitions :: NFATransitions
    , stateCount  :: Int }

emptyEnv :: Env
emptyEnv = Env
    { transitions = DMap.empty (DMap.empty [])
    , stateCount  = 0 }

compileNFA :: Regex -> S.State Env NFA
compileNFA reg = do
    (start, end) <- constructNFA reg
    ts <- S.gets transitions
    return $ NFA start end ts

fromRegex :: Regex -> NFA
fromRegex reg = fst $ S.runState (compileNFA reg) emptyEnv

eps :: Char
eps = '\949'

-- Construction of Epsilon-NFA
-- TODO: is this wrong?
constructNFA :: Regex -> S.State Env (State, State)
constructNFA Epsilon = do
    currentState <- getNextState
    nextState <- getNextState
    createEdge currentState nextState eps
    return (currentState, nextState)

constructNFA Dot = do
    currentState <- getNextState
    nextState <- getNextState
    createEdge currentState nextState '.'
    return (currentState, nextState)

constructNFA (Literal l) = do
    currentState <- getNextState
    nextState <- getNextState
    createEdge currentState nextState l
    return (currentState, nextState)

constructNFA (Kleene exp) = do
    currentState <- getNextState
    localFinalState <- getNextState

    -- create the inner automaton
    (innerStart, innerEnd) <- constructNFA exp

    -- connect current state with inner start
    createEdge currentState innerStart eps

    -- connect innerEnd with inner start
    createEdge innerEnd innerStart eps

    -- connect innerEnd with localFinal
    createEdge innerEnd localFinalState eps

    -- connect current state with localFinal
    createEdge currentState localFinalState eps

    return (currentState, localFinalState)

constructNFA (Concat l r) = do
    (lStart, lEnd) <- constructNFA l
    (rStart, rEnd) <- constructNFA r

    -- connect lEnd to rStart
    createEdge lEnd rStart eps

    return (lStart, rEnd)


createEdge :: State -> State -> Char -> S.State Env ()
createEdge origin target c =
    S.modify(\e ->
        e{ transitions = DMap.insert origin
            (DMap.insert c (target :) )
            (transitions e)
        })

getNextState :: S.State Env State
getNextState = do
    s <- S.get
    let c = stateCount s
    S.modify (const s{ stateCount = c + 1})
    return c

-- Manually insert the accepting-state as the epsilon closure is reflexive and
-- it would otherwise not be part of it since it has an outdegree of 0.
emptyDFSState :: State -> (DefaultMap State [State], [State])
emptyDFSState initial = (DMap.add initial [initial] (DMap.empty []), [])

-- | Compute the epsilon clojure of a given Epsilon-NFA
epsilonClosure :: NFA -> EpsClosure
epsilonClosure (NFA start end (outer,d)) = DMap.add end [end] (go adjacencyEpsList)
    where 
        adjacencyEpsList :: DefaultMap State [State]
        adjacencyEpsList = DMap.create (Map.fromList $ map (\(s,ts) -> (s, DMap.lookup eps ts)) (Map.toList outer)) []

        go :: DefaultMap State [State] -> DefaultMap State [State]
        go m = DMap.create (foldr
                (\elem accu -> Map.union accu
                    (fst . fst $ S.execState (dfs elem elem m) (emptyDFSState elem)))
                Map.empty (DMap.keys m)
            ) []

dfs :: State -> State -> DefaultMap State [State] -> S.State (DefaultMap State [State], [State]) ()
dfs target current adjacency = do
    (accu, seen) <- S.get
    if current `elem` seen then
        return ()
    else do
        let seen' = current:seen
        S.modify (\(accu', _) -> (DMap.insert target (current:) accu', seen'))
        let neighbours = DMap.lookup current adjacency
        mapM_ (\n -> dfs target n adjacency ) neighbours
