module DFA
    ( fromNFA
    , fromNFAMulti
    , flattenToDFA
    ) where

import DMap (DefaultMap)
import Data.Map (Map)
import qualified DMap
import qualified Data.Map as Map

import Datatypes
import NFA (epsilonClosure)
import Data.List (union, intercalate)

import Control.Monad (when)
import qualified Control.Monad.State as S

fromNFA :: NFA -> DFA
fromNFA = flattenToDFA . fromNFAMulti 

flattenToDFA :: PowerSetDFA -> DFA
flattenToDFA (PowerSetDFA start accepts ts) =
    let newLabels = newLabelings (DMap.keys ts) 0 (DMap.empty $ -1) in 
        DFA (DMap.lookup start newLabels)
            (map (`DMap.lookup` newLabels) accepts)
            (foldr
                (\key accu -> DMap.union accu (updateKey key newLabels $ DMap.empty Map.empty))
                (DMap.empty Map.empty)
                (DMap.keys ts)
            )
    where
        newLabelings :: [MultiState] -> Int -> DefaultMap MultiState State -> DefaultMap MultiState State
        newLabelings [] _ labelings     = labelings
        newLabelings (l:ls) n labelings = newLabelings ls (n+1) (DMap.add l n labelings)
        updateKey :: MultiState -> DefaultMap MultiState State -> DFATransitions -> DFATransitions
        updateKey key labeling accu =
            -- compute the new values
            let newValues = Map.map (\ms -> DMap.lookup ms labeling) (DMap.lookup key ts) in 
            -- insert newValues into accu at newLabel
            DMap.add (DMap.lookup key labeling) newValues accu -- compute the new keys

            where
                updateAllValues :: Map Char MultiState -> DefaultMap MultiState State -> Map Char State
                updateAllValues tsForChar newLabelings = Map.map (\val -> DMap.lookup val newLabelings) tsForChar

                getSymbols :: PowerSetDFATransitions -> MultiState -> [Char]
                getSymbols ts ms = Map.keys (DMap.lookup ms ts)

data Env = Env
    { acceptingStates :: [MultiState]
    , transitions     :: PowerSetDFATransitions
    , seen            :: [MultiState] }

emptyEnv :: Env
emptyEnv = Env
    { acceptingStates = []
    , transitions     = DMap.empty Map.empty
    , seen            = [] }

eps :: Char
eps = '\949'

fromNFAMulti :: NFA -> PowerSetDFA
fromNFAMulti nfa@(NFA start _ ts) =
    PowerSetDFA initStart (acceptingStates finalState) (transitions finalState)
    where
        epsClosure = epsilonClosure nfa
        initStart  = DMap.lookup start epsClosure
        finalState = S.execState (fromNFAMulti' (nfa, epsClosure) initStart) emptyEnv

fromNFAMulti' :: (NFA, EpsClosure) -> MultiState -> S.State Env ()
fromNFAMulti' dtype@(nfa@(NFA start _ ts),epsClosure) currentState = do
    -- Mark current state as seen
    S.modify (\s -> s{seen = currentState:seen s})

    let symbols = getSymbols ts currentState
    let reachableStates = foldr (\char accu -> Map.insert char (getReachableStates (ts, currentState) char) accu) Map.empty symbols
    let reachableStatesWithClosure = Map.map (\multiState -> getMSClosure (epsClosure, multiState)) reachableStates 

    -- Mark all the computed multistates as reachable for the current state by putting them into `transitions`
    S.modify(\s -> s{transitions = DMap.add currentState reachableStatesWithClosure (transitions s)})

    -- If an accepting state is part of the `currentState`, put `currentstate` into env
    when (hasAcceptState (nfa, currentState)) $ S.modify (\s-> s{ acceptingStates = currentState : acceptingStates s } )

    -- Call recursively on reachable multistates which have not been seen yet
    seen' <- S.gets seen
    let newStates = filter (`notElem` seen') (Map.elems reachableStatesWithClosure)
    mapM_ (fromNFAMulti' dtype) newStates

getSymbols :: NFATransitions -> MultiState -> [Char]
getSymbols nfaTs = foldr (\state accu -> accu ++ filter (/= eps) (DMap.keys (DMap.lookup state nfaTs))) []

getReachableStates :: (NFATransitions, MultiState) -> Char -> MultiState
getReachableStates (nfaTs, ms) c = foldr (\state accu -> DMap.lookup c (DMap.lookup state nfaTs) ++ accu) [] ms

getMSClosure :: (EpsClosure, MultiState) -> MultiState 
getMSClosure (epsC, ms) = foldr (\state accu -> accu `union` DMap.lookup state epsC) [] ms

hasAcceptState :: (NFA, MultiState) -> Bool
hasAcceptState (_, [])                                   = False
hasAcceptState (nfa@(NFA _ accept _),s:ss) | accept == s = True
                                           | otherwise   = hasAcceptState (nfa, ss)

