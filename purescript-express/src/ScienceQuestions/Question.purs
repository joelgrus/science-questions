module ScienceQuestions.Question
  ( Question()
  , Answer()
  , randomQuestion
  ) where

import           Control.Monad.Eff
import           Control.Monad.Eff.Random (RANDOM(), randomInt)
import           Data.Array (replicateM)
import           Prelude

import           ScienceQuestions.Token

-- | If I were more clever, I would figure out a way to share code with the
-- | client-side PureScript.
newtype Question = Question
  { questionText  :: String
  , answers       :: Array Answer
  , correctAnswer :: Int
  }

instance showQuestion :: Show Question where
  show (Question q) = show q.questionText ++ ": "  ++
    show q.answers ++ " " ++ show q.correctAnswer

-- | Just a helper function so that we can lift Question creation into Eff.
question :: String -> Array Answer -> Int -> Question
question text answers correct = Question {
  questionText: text,
  answers: answers,
  correctAnswer: correct
}

type Answer = String

-- | Just some shorthand for a row of effects that contains RANDOM.
type RandomEffect eff = (random :: RANDOM | eff)

-- | Generate a random question in a context that allows for RANDOM effects.
randomQuestion :: forall eff. Int ->
                              GetNextToken (RandomEffect eff) ->
                              GetNextToken (RandomEffect eff) ->
                              Eff (RandomEffect eff) Question
randomQuestion numAnswers getNextQuestionToken getNextAnswerToken =
  question <$> generateString getNextQuestionToken
           <*> replicateM numAnswers (generateString getNextAnswerToken)
           <*> randomInt 0 (numAnswers - 1)
