-- Main.hs for the Regex module (executable)
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Regex
import Datatypes
import RDSL
import Web.Scotty
import Data.Aeson
import Network.Wai.Middleware.Cors

import Data.Text.Lazy as TL (pack)
import DMap                 (toString)
import NFA                  (epsilonClosure,fromRegex)
import DFA                  (fromNFAMulti,flattenToDFA)
import Network.HTTP.Types   (hContentType)

data Request = Request { regexp, input :: String }

instance FromJSON Request where 
    parseJSON (Object v) = Request
        <$> v .: "regexp"
        <*> v .: "input"
    parseJSON _ = fail "Error parsing Request Object."

type MatchStatus = Bool
type ValidState  = Bool
type Input       = String

data Response = Response 
    { graphviz :: String
    , matched  :: MatchStatus
    , trace    :: [(State,ValidState)] }

instance ToJSON Response where
    toJSON(Response{graphviz=g,matched=m,trace=t}) =
        object ["graphviz" .= g, "matched" .= m, "trace" .= t]

corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy
    { corsOrigins        = Nothing
    , corsMethods        = ["GET", "POST", "DELETE", "OPTIONS"]
    , corsRequestHeaders = [hContentType, "Authorization", "X-Requested-With"]
    , corsExposedHeaders = Nothing
    , corsMaxAge         = Nothing
    , corsVaryOrigin     = False
    , corsRequireOrigin  = False
    , corsIgnoreFailures = False }

main :: IO ()
main = do
    scotty 8080 $ do
        middleware $ cors (const $ Just corsPolicy)
        get "/" $ text "Welcome to our Regex-Visualizer!"
        post  "/" $ do
            resp <- processRequest <$> jsonData
            json resp
        notFound $ text "404: Page not found."
    
processRequest :: Request -> Response
processRequest (Request{regexp=trex, input=i}) =
    let reg          = read trex -- TODO: add parsing state back.
        nfa          = fromRegex reg
        epsClosure   = epsilonClosure nfa
        powerSetDFA  = fromNFAMulti nfa
        dfa          = flattenToDFA powerSetDFA
        (matched,tr) = Regex.checkWithTrace dfa i
    in Response
        { graphviz = show dfa
        , matched  = matched
        , trace    = tr }