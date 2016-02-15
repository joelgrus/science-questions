{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import Control.Applicative (liftA2)
import Control.Monad (guard, liftM, replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BS
import Data.List (unfoldr)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (simpleCors)
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
data Token = Start | Stop | Word String deriving (Eq, Ord)

-- | As part of our deserialization, we'll need to be able to create Tokens
-- | from Strings, so we need a Read instance.
instance Read Token where
  readsPrec _ "__START__" = [(Start,  "")]
  readsPrec _ "__STOP__"  = [(Stop,   "")]
  readsPrec _ w           = [(Word w, "")]

-- | Given a token, find a next token in an effectful way. (Typically random.)
type GetNextToken = Token -> IO Token

tokensFrom :: Token -> GetNextToken -> IO [Token]
tokensFrom startToken getNext = do
  nextToken <- getNext startToken   -- nextToken :: Token
  case nextToken of
    Stop  -> return []
    token -> liftA2 (:) (pure token) (tokensFrom token getNext)

-- | We don't want to use `unwords` because we don't want to put spaces in
-- | front of punctuation. This is a somewhat hacky replacement.
smartJoin :: [Token] -> String
smartJoin = dropWhile (== ' ') . concat . addSeparators
  where
    addSeparators = concatMap addSeparator
    addSeparator token = case token of
      Word w | w `elem` ["?", ",", "."] -> ["",  w]
      Word w                            -> [" ", w]
      _                                 -> []

generate :: GetNextToken -> IO String
generate = fmap smartJoin . tokensFrom Start

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

-- | Load some (JSON-serialized) transitions from a file. `decode`
-- | will return a `Maybe (M.map Text [Text])`, which we then need to pull out
-- | of the Maybe and convert to a M.map Token [Token]
loadTransitions :: String -> IO Transitions
loadTransitions = fmap (textToTokens . fromJust . decode) . BS.readFile
  where textToTokens = M.map (map read) . M.mapKeys read

questionTransitions :: IO Transitions
questionTransitions = loadTransitions "questions.json"

answerTransitions :: IO Transitions
answerTransitions = loadTransitions "answers.json"

-- | Pick a random element of a list, which better not be empty!
pick :: [a] -> IO a
pick xs = do
  idx <- randomRIO (0, length xs - 1) -- choose a random index
  return (xs !! idx)                  -- return that element of the list

-- | An implementation of GetNextToken based on a Transitions map.
randomNextToken :: Transitions -> GetNextToken
randomNextToken transitions token =
  case M.lookup token transitions of
    Just tokens -> pick tokens
    _           -> return Stop

type API = "question" :> Get '[JSON] Question

getRandomQuestionUsingTransitions :: IO Question
getRandomQuestionUsingTransitions = do
  qt <- questionTransitions
  at <- answerTransitions
  randomQuestion 4 (randomNextToken qt) (randomNextToken at)

server :: Server API
server = liftIO getRandomQuestionUsingTransitions

startApp :: IO ()
startApp = run 8080 $ simpleCors $ app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy
