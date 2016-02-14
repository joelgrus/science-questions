module Main where

import Prelude hiding (div)

import Control.Bind ((=<<))
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Array (concat, filter, length, zip, (..), modifyAt, snoc)
import Data.Array.Unsafe (unsafeIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.Foldable (foldMap)
import Data.Foreign
import Data.Foreign.Class (IsForeign, readProp, readJSON)
import Data.List (singleton)
import Data.String (joinWith)
import Data.Tuple (fst, snd, uncurry, Tuple(..))
import Network.HTTP.Affjax (AJAX(), get)
import Pux
import Pux.DOM.HTML.Elements (a, img, div, p, button, text)
import Pux.DOM.HTML.Attributes (href, src, onClick, send, className)
import Pux.Render.DOM
import qualified Signal.Channel as S

questionServiceUrl :: String
--questionServiceUrl = "http://localhost:8080/question"
questionServiceUrl = "http://54.174.99.38/question"

-- | We define a newtype for Question so we can create an IsForeign instance.
newtype Question = Question {
  questionText  :: String,
  answers       :: Array Answer,
  correctAnswer :: AnswerId,
  chosenAnswer  :: Maybe AnswerId  -- which answer the player clicked on
}

type QuestionId = Int
type Answer = String
type AnswerId = Int

-- | Deserializing from JSON is pretty straightforward, we just need to add an
-- | extra Nothing value for chosenAnswer.
instance questionIsForeign :: IsForeign Question where
  read value = do
    questionText  <- readProp "questionText"  value
    answers       <- readProp "answers"       value
    correctAnswer <- readProp "correctAnswer" value
    return $ Question {
      questionText : questionText,
      answers : answers,
      correctAnswer : correctAnswer,
      chosenAnswer : Nothing
    }

data Action =
    NewGame                          -- start a new game
  | ClickAnswer QuestionId AnswerId  -- click on an answer
  | QuestionReceived Question        -- receive an AJAX response with a question

type State = {
  score              :: Int,
  questions          :: Array Question,
  waitingForQuestion :: Boolean -- are we waiting for an AJAX call to return?
}

-- | Start out with no questions.
initialState :: State
initialState = { score: 0, questions: [], waitingForQuestion: true }

-- | When you click on questionId answerId, we need to update the score,
-- | set the chosenAnswer field for that question, and set the
-- | waitingForQuestion flag.
answerClicked :: QuestionId -> AnswerId -> State -> State
answerClicked questionId answerId state =
  { score : newScore, questions: newQuestions, waitingForQuestion: true }
  where
    isCorrect = case state.questions `unsafeIndex` questionId of
      Question q | q.correctAnswer == answerId -> true
      _ -> false
    newScore = if isCorrect then state.score + 1 else state.score
    newQuestions = fromJust $ modifyAt questionId updateChosen state.questions
    updateChosen (Question q) = Question $ q { chosenAnswer = Just answerId }

-- | we only want to *actually* add the question if we're waiting for it.
-- | otherwise, we just return the state as is
appendQuestion :: Question -> State -> State
appendQuestion question state =
  if state.waitingForQuestion
  then state { questions = snoc state.questions question,
               waitingForQuestion = false }
  else state

-- How to update the state (and perform effects) for each action type.
update :: forall eff. Update (ajax    :: AJAX,
                              err     :: EXCEPTION,
                              console :: CONSOLE    | eff) State Action
update action state input =
  case action of
    -- for a NewGame action, we need to reset the state to the initialState
    -- and make an AJAX request for a new question
    NewGame ->
      { state: initialState
      , effects: [ requestQuestion ] }
    -- for a click on an answer, update the state with the clicked answer,
    -- and make an AJAX request for a new question
    ClickAnswer questionId answerId ->
      { state: answerClicked questionId answerId state
      , effects: [ requestQuestion ]
      }
    -- for the result of a new question AJAX call, try to update the state
    QuestionReceived question ->
      { state: appendQuestion question state
      , effects: [] }
  where
    -- AJAX boilerplate that I don't totally understand
    requestQuestion =
      launchAff $ do
        res <- get questionServiceUrl
        let question = readJSON res.response :: F Question
        liftEff $ case question of
          (Left err) -> log "Error parsing JSON!"
          (Right question) -> S.send input (singleton (QuestionReceived question))

-- | An answer just gets rendered as a single div with classes to indicate
-- | whether its question has been answered, whether it's the right answer, and
-- | whether it's the chosen answer. If the question hasn't been answered yet,
-- | we add a click handler as well.
renderAnswer :: Question -> QuestionId -> Answer -> AnswerId -> VirtualDOM
renderAnswer (Question q) questionId answer answerId =
  div ! className classes ! clickHandlerIfUnanswered $ text answer
  where
    isAnswered = isJust q.chosenAnswer
    isChosen   = isAnswered && answerId == fromJust q.chosenAnswer
    isCorrect  = isAnswered && answerId == q.correctAnswer

    classes = joinWith " " $ map snd $ filter fst [
      Tuple true                          "answer",
      Tuple (isAnswered && isChosen)      "chosen",
      Tuple (isAnswered && isCorrect)     "correct",
      Tuple (isAnswered && not isCorrect) "wrong"
    ]

    clickHandlerIfUnanswered =
      if isAnswered
      then Attrs [] [] -- no-op "attribute"
      else onClick (send $ ClickAnswer questionId answerId)

-- | VirtualDOM is a Monoid, so if we have an array of inputs `xs` and some
-- | function that maps each `x` to a VirtualDOM element, we can use `foldMap`
-- | to combine the results into a single VirtualDOM element. It turns out we'll
-- | need both an element and its index to create a VirtualDOM element; this
-- | helper function provides that `map`
foldMapWithIndex :: forall a. (a -> Int -> VirtualDOM) -> Array a -> VirtualDOM
foldMapWithIndex f xs = foldMap (uncurry f) pairs
  where pairs = zip xs (0 .. (length xs - 1))

-- | We render a question by displaying its questionText and then rendering its
-- | answers.
renderQuestion :: Question -> QuestionId -> VirtualDOM
renderQuestion (Question q) questionId = div $ do
  p ! className "question" $ text $ questionNumber ++ q.questionText
  foldMapWithIndex (renderAnswer (Question q) questionId) q.answers
  where
    questionNumber = show (questionId + 1) ++ ". "

view :: State -> VirtualDOM
view state = div $ do
  div ! className "sidebar" $ do
    p ! className "score" $ text ("Score: " ++ show state.score)
    button ! onClick (send NewGame) $ text "New Game"
    p ! className "twitter" $
      a ! href "http://twitter.com/joelgrus" $ text "@joelgrus"
    p ! className "github" $
      a ! href "https://github.com/joelgrus/science-questions" $
        img ! src "octocat.png"
  foldMapWithIndex renderQuestion state.questions

main = renderToDOM "#app" =<< app
  { state: initialState
  , update: update
  , view: view
  , inputs: []
  }
