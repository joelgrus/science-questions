module Main where

import           Control.Monad.Eff.Class
import           Control.Monad.Eff.Console
import           Control.Monad.Eff.Random (RANDOM())
import           Data.Foreign.EasyFFI
import           Data.Maybe.Unsafe (fromJust)
import           Node.Express.App
import           Node.Express.Handler
import           Node.Express.Response
import           Prelude hiding (apply)

import ScienceQuestions.Question
import ScienceQuestions.Transitions

-- | The handler for the /question endpoint. Returns a random question.
type Handler e = HandlerM (express :: EXPRESS | e) Unit
-- |    sendJson :: forall e a. a -> Handler e
questionHandler :: forall e. Transitions ->
                             Transitions ->
                             Handler (random :: RANDOM | e)
questionHandler qTransitions aTransitions = do
  -- create a random question (in the Eff context) and lift it into the
  -- Handler context
  question <- liftEff $ randomQuestion 4 (randomNextToken qTransitions)
                                         (randomNextToken aTransitions)
  sendJson question

appSetup :: forall e. Transitions ->
                      Transitions ->
                      App (random :: RANDOM, console :: CONSOLE | e)
appSetup qTransitions aTransitions = do
  liftEff $ log "Setting up"
  -- add our handler at the "/question" endpoint
  get "/question"   (questionHandler qTransitions aTransitions)

main = do
  qTransitions <- fromJust <$> loadTransitions "questions.json"
  aTransitions <- fromJust <$> loadTransitions "answers.json"
  port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
  listenHttp (appSetup qTransitions aTransitions) port \_ ->
    log $ "Listening on " ++ show port
