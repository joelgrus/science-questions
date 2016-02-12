module Main where

import Prelude hiding (div)

import Control.Bind ((=<<))
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Array (length, zip, (..))
import Data.Array.Unsafe (unsafeIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.Monoid (Monoid, mempty)
import Data.Foldable (foldl)
import Data.Foreign
import Data.Foreign.Class (IsForeign, readProp, readJSON)
import Data.List (singleton)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX(), get)
import Pux
import Pux.DOM.HTML.Elements (div, p, button, text)
import Pux.DOM.HTML.Attributes (onClick, send, className)
import Pux.Render.DOM
import qualified Signal.Channel as S

questionServiceUrl :: String
--questionServiceUrl = "http://localhost:8080/question"
questionServiceUrl = "http://54.174.99.38/question"

newtype Answer = Answer {
  answerId :: Int,
  answerText :: String
}

newtype Question = Question {
  questionId :: Int,
  questionText :: String,
  answers :: Array Answer,
  correctAnswer :: Int,
  chosenAnswer :: Maybe Int
}

instance questionIsForeign :: IsForeign Question where
  read value = do
    questionText <- readProp "questionText" value
    rawAnswers <- readProp "answers" value
    correctAnswer <- readProp "correctAnswer" value
    let indexedAnswers =
      map (\(Tuple a i) -> Answer { answerId: i, answerText: a })
          (zip rawAnswers (0 .. (length rawAnswers - 1)))
    return $ Question {
      questionId : -1,
      questionText : questionText,
      answers : indexedAnswers,
      correctAnswer : correctAnswer,
      chosenAnswer : Nothing
    }


data Action = NewGame | ClickAnswer Int Int | QuestionReceived Question

type State = {
  score :: Int,
  questions :: Array Question,
  waitingForQuestion :: Boolean
}

fakeQuestion :: Question
fakeQuestion = Question {
  questionId: 0,
  questionText: "Is this a fake question?",
  answers: [
    Answer { answerId: 0, answerText: "yes" },
    Answer { answerId: 1, answerText: "no" },
    Answer { answerId: 2, answerText: "maybe" }
  ],
  correctAnswer: 0,
  chosenAnswer: Nothing
}

initialState :: State
initialState = { score: 0, questions: [], waitingForQuestion: false }

answerClicked :: Int -> Int -> State -> State
answerClicked questionId answerId state =
  { score : newScore, questions: newQuestions, waitingForQuestion: true }
  where
    isCorrect = case state.questions `unsafeIndex` questionId of
      Question q | q.correctAnswer == answerId -> true
      _ -> false
    newScore = if isCorrect then state.score + 1 else state.score
    newQuestions = map (\(Question q) ->
      if q.questionId == questionId
        then Question $ q { chosenAnswer = Just answerId }
        else Question q) state.questions


update :: forall eff. Update (ajax :: AJAX,
                              err :: EXCEPTION,
                              console :: CONSOLE| eff) State Action
update action state input =
  case action of
    NewGame ->
      { state: initialState
      , effects: [ do log "newgame", requestQuestion ] }
    ClickAnswer questionId answerId ->
      { state: answerClicked questionId answerId state
      , effects: [
          do log ("click" ++ (show questionId) ++ (show answerId)),
          requestQuestion
        ]
      }
    QuestionReceived question ->
      { state: addQuestion question state
      , effects: [] }
  where
    requestQuestion =
      launchAff $ do
        res <- get questionServiceUrl
        let question = readJSON res.response :: F Question
        liftEff $ case question of
          (Left err) -> log "Error parsing JSON!"
          (Right question) -> S.send input (singleton (QuestionReceived question))

addQuestion :: Question -> State -> State
addQuestion (Question question) state =
  state { questions = state.questions ++ [questionWithId] }
  where
    questionWithId = Question $ question { questionId = questionId }
    questionId = length state.questions

renderQuestions :: Array Question -> VirtualDOM
renderQuestions questions = mconcat $ map (\(Question q) -> do
  p ! className "question" $ text (q.questionText)
  let isAnswered = isJust q.chosenAnswer
  mconcat $ map (\(Answer a) -> do
    let isChosen = isAnswered && a.answerId == fromJust q.chosenAnswer
    let isCorrect = isAnswered && q.correctAnswer == a.answerId
    div ! className (answerClasses [isAnswered, isCorrect, isChosen])
        ! onClick (send $ ClickAnswer q.questionId a.answerId)
        $ text a.answerText
    ) q.answers
  ) questions
  where
    answerClasses :: Array Boolean -> String
    answerClasses [false, _, _] = "answer"
    answerClasses [true, true, false] = "answer correct"
    answerClasses [true, true, true] = "answer correct chosen"
    answerClasses [true, false, false] = "answer wrong"
    answerClasses [true, false, true] = "answer wrong chosen"
    answerClasses _ = ""

mconcat :: forall m. (Monoid m) => Array m -> m
mconcat = foldl append mempty

view :: State -> VirtualDOM
view state = div $ do
  div ! className "right-side" $ do
    p ! className "score" $ text ("Score: " ++ show state.score)
    button ! onClick (send NewGame) $ text "new game"
  renderQuestions state.questions

main = renderToDOM "#app" =<< app
  { state: initialState
  , update: update
  , view: view
  , inputs: []
  }
