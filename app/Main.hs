-- Main.hs for the Regex module (executable)
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Datatypes
import RDSL
import Web.Scotty
import Data.Aeson
import Data.Text.Lazy as TL (pack)
import DMap   (toString)
import DFA    (fromNFAMulti, flattenToDFA)
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Status
import NFA    (epsilonClosure, fromRegex)
import Debug.Trace (trace)

-- import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import qualified Regex

data Request = Request
    { regexp :: Maybe String
    , input :: Maybe String }

instance FromJSON Request where 
    parseJSON (Object v) = Request
        <$> v .:? "regexp"
        <*> v .:? "input"
    parseJSON _ = fail "Error parsing Request Object."


main :: IO ()
main = do
    scotty 8080 $ do 
        middleware simpleCors

        options (regex ".*") $ do
            trace "in options" (return ())
            status status200

        get "/" $ do 
            trace "in get /" (return ())
            setHeader (TL.pack "Access-Control-Allow-Origin") (TL.pack "*")
            setHeader (TL.pack "Access-Control-Allow-Methods") (TL.pack "DELETE, POST, GET, OPTIONS")
            setHeader (TL.pack "Access-Control-Allow-Headers") (TL.pack "Content-Type, Authorization, X-Requested-With")

            text "blabla"

        post  "/" $ do 
            trace "in post /" (return ())
            Request{regexp=r, input=i} <- jsonData
            -- processing..

            setHeader (TL.pack "Access-Control-Allow-Origin") (TL.pack "*")
            text $ TL.pack ((show r) ++ (show i))

        notFound $ do 
            trace "in not-found" (return ())
            text "Page not found. "



    {-
    let reg   = url
        input = "https://discord.com/channels/me/1435605061844860999"

    let nfa         = fromRegex reg
    let epsClosure  = epsilonClosure nfa
    let powerSetDFA = fromNFAMulti nfa
    let dfa         = flattenToDFA powerSetDFA

    -- Save to files (locally)
    -- writeFile "nfa.dot" $ show nfa
    -- writeFile "powerSetDFA.dot" $ show powerSetDFA
    -- writeFile "dfa.dot" $ show dfa

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


-}