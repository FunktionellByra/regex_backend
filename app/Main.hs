-- Main.hs for the Regex module (executable)

module Main where

import Datatypes
import RDSL
import DMap   (toString)
import DFA    (fromNFAMulti, flattenToDFA)
import NFA    (epsilonClosure, fromRegex)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import qualified Regex

main :: IO () -- TODO: use MaybeT IO ()
main = do
    hSetBuffering stdout NoBuffering

    let reg   = symbol 'S' +++ eps
        input = "a1"

    let nfa         = fromRegex reg
    let epsClosure  = epsilonClosure nfa
    let powerSetDFA = fromNFAMulti nfa
    let dfa         = flattenToDFA powerSetDFA

    -- Save to files (locally)
    writeFile "nfa.dot" $ show nfa
    writeFile "powerSetDFA.dot" $ show powerSetDFA
    writeFile "dfa.dot" $ show dfa

    let (matched, trace) = Regex.checkWithTrace dfa input
    print matched

    -- case parseReg p of
    --     Just reg -> do 
    --         putStrLn $ "Token received: " ++ show reg

    --         let nfa         = fromRegex reg
    --         let epsClosure  = epsilonClosure nfa
    --         let powerSetDFA = fromNFAMulti nfa
    --         let dfa         = flattenToDFA powerSetDFA

    --         -- Save to files (locally)
    --         writeFile "nfa.dot" $ show nfa
    --         writeFile "powerSetDFA.dot" $ show powerSetDFA
    --         writeFile "dfa.dot" $ show dfa

    --         putStr ">? "
    --         input <- getLine
    --         -- let matched = Regex.match1 reg input

    --         -- Enables trace (experimental)
    --         let (matched, trace) = Regex.checkWithTrace dfa input
    --         putStrLn $ "Checking " ++ show input ++ " on " ++ show p ++
    --                    " results in: " ++ show matched
    --         putStrLn $ "The trace is: " ++ show trace
    --     Nothing -> putStrLn "Failed to parse."
    -- main -- continue