{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import Control.Applicative (liftA2)
import Control.Monad (liftM, replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import System.IO.Memoize (eagerlyOnce)
import System.Random (randomRIO)

-- | First, the machinery around generating questions.

type Answer = String

data Question = Question
  { questionText  :: String
  , answers       :: [Answer]
  , correctAnswer :: Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Question)

data Token = Start | Stop | Word String deriving (Eq, Ord)

-- | As part of our deserialization, we'll need to be able to create Tokens
-- | from Strings, so we need a Read instance.
instance Read Token where
  readsPrec _ "__START__" = [(Start,  "")]
  readsPrec _ "__STOP__"  = [(Stop,   "")]
  readsPrec _ w           = [(Word w, "")]

-- | Given a token, finds a next token in an effectful way. (Typically random.)
type GetNextToken = Token -> IO Token

-- | Given a starting token, and an effectful getNext function, returns the list
-- | of tokens resulting from calling getNext until it reaches Stop.
tokensFrom :: Token -> GetNextToken -> IO [Token]
tokensFrom startToken getNext = do
  nextToken <- getNext startToken   -- nextToken :: Token
  case nextToken of
    Stop  -> return []
    token -> liftA2 (:) (pure token) (tokensFrom token getNext)

-- | Converts a list of tokens into a string, putting spaces between words
-- | but not before punctuation.
smartJoin :: [Token] -> String
smartJoin = dropWhile (== ' ') . concat . addSeparators
  where
    addSeparators = concatMap addSeparator
    addSeparator token = case token of
      Word w | w `elem` ["?", ",", "."] -> ["",  w]
      Word w                            -> [" ", w]
      _                                 -> []

-- | Given an effectful nextToken function, returns a randomly generated sentence.
generate :: GetNextToken -> IO String
generate = fmap smartJoin . tokensFrom Start

-- | Generates a random Question, given an Int indicating how many answers to
-- | generate, a GetNextToken function for the questions themselves,
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

-- | Loads some (JSON-serialized) transitions from a file. `decode`
-- | will return a `Maybe (M.map Text [Text])`, which we then need to pull out
-- | of the Maybe and convert to a M.map Token [Token]
loadTransitions :: String -> IO Transitions
loadTransitions = fmap (textToTokens . fromJust . decode) . BS.readFile
  where textToTokens = M.map (map read) . M.mapKeys read

questionTransitions :: IO Transitions
questionTransitions = loadTransitions "questions.json"

answerTransitions :: IO Transitions
answerTransitions = loadTransitions "answers.json"

-- | Picks a random element of a list, which better not be empty!
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

-- | And now we have to implement the servant parts.

type API = "question" :> Get '[JSON] Question

startApp :: IO ()
startApp = do
  -- loading / creating the transitions is expensive, so we do it eagerlyOnce
  cachedQt <- eagerlyOnce questionTransitions
  cachedAt <- eagerlyOnce answerTransitions
  run 8080 $ simpleCors $ app cachedQt cachedAt

app :: IO Transitions -> IO Transitions -> Application
app cachedQt cachedAt = serve api (server cachedQt cachedAt)

server :: IO Transitions -> IO Transitions -> Server API
server cachedQt cachedAt = liftIO $ do
  qt <- cachedQt
  at <- cachedAt
  randomQuestion 4 (randomNextToken qt) (randomNextToken at)

api :: Proxy API
api = Proxy
