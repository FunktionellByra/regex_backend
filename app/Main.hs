-- Main.hs for the Regex module (executable)
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Regex
import Datatypes
import Parser
import Web.Scotty
import Data.Aeson
import Network.Wai.Middleware.Cors

import Data.Text.Lazy as TL (pack)
import DMap                 (toString)
import NFA                  (epsilonClosure,fromRegex)
import DFA                  (fromNFAMulti,flattenToDFA)
import Network.HTTP.Types   (hContentType,status400)
import qualified Debug.Trace as D

data Request = Request { regexp :: String,input :: String }

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
            req <- jsonData
            processRequest req
        notFound $ text "404: Page not found."
    
processRequest :: Request -> ActionM () -- Response
processRequest (Request{regexp=reg,input=i}) = do
    case parseRegex reg of 
        Right r -> do
            let nfa          = fromRegex r
                epsClosure   = epsilonClosure nfa
                powerSetDFA  = fromNFAMulti nfa
                dfa          = flattenToDFA powerSetDFA
                (matched,tr) = checkWithTrace dfa i
                res = Response
                    { graphviz = show dfa
                    , matched  = matched
                    , trace    = tr }
            json res
        Left e -> do 
            status status400
            text $ TL.pack $ "Invalid pattern for regular expression: " ++ show e
