-- Main.hs for the Regex module (executable)
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Regex
import Datatypes
import Parser
import Web.Scotty
import Data.Aeson
import Network.Wai.Middleware.Cors
import qualified Data.ByteString.Char8 as BS
import Network.Wai.Handler.Warp (setHost, setPort, defaultSettings)

import Data.Text.Lazy as TL       (pack)
import DMap                       (toString)
import NFA                        (epsilonClosure,fromRegex)
import DFA                        (fromNFAMulti,flattenToDFA)
import Network.HTTP.Types         (hContentType,status400)
import qualified Network.Wai as W (Request,requestHeaders)

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
    { graphvizNFA :: String
    , graphvizDFA :: String
    , matched  :: MatchStatus
    , trace    :: [(String, State,ValidState)] }

instance ToJSON Response where
    toJSON(Response{graphvizNFA=g1,graphvizDFA=g2,matched=m,trace=t}) =
        object ["graphvizNFA" .= g1, "graphvizDFA" .= g2, "matched" .= m, "trace" .= t]

isAllowedOrigin :: BS.ByteString -> Bool
isAllowedOrigin origin = origin ==
    "https://funktionellbyra.github.io"        ||
    "http://localhost:" `BS.isPrefixOf` origin ||
    "http://127.0.0.1:" `BS.isPrefixOf` origin

corsPolicy :: BS.ByteString -> CorsResourcePolicy
corsPolicy origin = CorsResourcePolicy
    { corsOrigins        = Just ([origin], False)
    , corsMethods        = ["GET","POST"]
    , corsRequestHeaders = [hContentType,"Authorization","X-Requested-With"]
    , corsExposedHeaders = Nothing
    , corsMaxAge         = Nothing
    , corsVaryOrigin     = True
    , corsRequireOrigin  = True
    , corsIgnoreFailures = False }

getCorsPolicy :: W.Request -> Maybe CorsResourcePolicy
getCorsPolicy req =
  case lookup "origin" (W.requestHeaders req) of
    Just origin | isAllowedOrigin origin -> Just (corsPolicy origin)
    _ -> Nothing

main :: IO ()
main = do
    -- host on 0.0.0.0 to expose it to network on server, non-verbose mode
    let opts = Options 0 (setPort 8080 $ setHost "0.0.0.0" $ defaultSettings) False
    scottyOpts opts $ do
        middleware $ cors getCorsPolicy
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
                    { graphvizNFA = show nfa
                    , graphvizDFA = show dfa
                    , matched  = matched
                    , trace    = tr }
            json res
        Left e -> do 
            status status400
            text $ TL.pack $ "Invalid pattern for regular expression: " ++ show e
