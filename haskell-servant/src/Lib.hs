{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import Control.Monad (liftM, replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BS
import Data.List (unfoldr)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Random (randomRIO)

-- | First, the machinery around generating questions.

type Answer = String

data Question = Question
  { questionText  :: String
  , answers       :: [Answer]
  , correctAnswer :: Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Question)

-- | We create a markov chain of tokens, which are just words.
type Token = String

-- | We have sentinel tokens indicating the start and stop of a sentence.
start :: Token
start = "__START__"

stop :: Token
stop = "__STOP__"

-- | Given a token, find a next token in an effectful way. (Typically random.)
type GetNextToken = Token -> IO Token

-- | Produce a sentence by generating tokens up until the 'stop' sentinel.
-- | (I bet there's a magical one-line way of doing this.)
generate :: GetNextToken -> IO String
generate nextAfter = loop start []
  where
    loop :: Token -> [Token] -> IO String
    loop prev tokens = do
      token <- nextAfter prev
      if token == stop
        then return $ smartJoin tokens
        else loop token (tokens ++ [token])

-- | We don't want to use `unwords` because we don't want to put spaces in
-- | front of punctuation. This is a somewhat hacky replacement.
smartJoin :: [Token] -> String
smartJoin = dropWhile (== ' ') . concat . addSeparators
  where
    addSeparators = concatMap addSeparator
    addSeparator word
      | word `elem` ["?", ",", "."] = ["",  word]
      | otherwise                   = [" ", word]

-- | And now we're set up to generate random Questions. We need to specify how
-- | many answers we want, a GetNextToken function for the questions themselves,
-- | and a second GetNextToken function for answers.
randomQuestion :: Int -> GetNextToken -> GetNextToken -> IO Question
randomQuestion numAnswers getNextQuestionToken getNextAnswerToken =
  Question <$> generate getNextQuestionToken
           <*> replicateM numAnswers (generate getNextAnswerToken)
           <*> randomRIO (0, numAnswers - 1)

-- In particular, we will be using our transition map in order to implement
-- getNextToken. This means we need to define a type for it, load it from a
-- file, and create functions for using it.

type Transitions = M.Map Token [Token]

-- | Pick a random element of a list, which better not be empty!
pick :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

-- | An implementation of GetNextToken based on a Transitions map.
randomNextToken :: Transitions -> GetNextToken
randomNextToken transitions token =
  case M.lookup token transitions of
    Just tokens -> pick tokens
    _           -> return stop

-- | Load some (JSON-serialized) transitions from a file
loadTransitions :: String -> IO Transitions
loadTransitions = liftM (fromJust . decode) . BS.readFile

questionTransitions :: IO Transitions
questionTransitions = loadTransitions "questions.json"

answerTransitions :: IO Transitions
answerTransitions = loadTransitions "answers.json"

-- | Just some CORS boilerplate so we can call this API from other IP addresses.
questionCors :: Middleware
questionCors = cors $ const (Just questionResourcePolicy)

questionResourcePolicy :: CorsResourcePolicy
questionResourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Nothing -- gives you /*
        , corsMethods = ["GET"]
        , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

type API = "question" :> Get '[JSON] Question

startApp :: IO ()
startApp = run 8080 $ questionCors $ app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = liftIO $ do
  qt <- questionTransitions
  at <- answerTransitions
  randomQuestion 4 (randomNextToken qt) (randomNextToken at)
