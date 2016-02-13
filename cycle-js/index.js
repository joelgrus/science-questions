import Rx from 'rx';
import Cycle from '@cycle/core';
import {a, div, button, makeDOMDriver} from '@cycle/dom';
import {makeHTTPDriver} from '@cycle/http';
import {Record, List} from 'immutable';

//const QUESTION_SERVICE_URL = 'http://localhost:8080/question';
const QUESTION_SERVICE_URL = 'http://54.174.99.38/question';

function tapAndLog(x) {
  console.log(x);
  return x;
}


// The signals received from external sources.
function intent(sources) {
  const { DOM, HTTP } = sources;

  // A click on the "New Game" button.
  const newGameAction$ = DOM.select('.new-game').events('click');

  // A click on one of the answers to a not-yet-answered question. Gets mapped
  // to the pair [questionId, answerId].
  const chooseAnswerAction$ = DOM.select('.answer.unanswered')
    .events('click')
    .map(ev => [ev.target['data-question-id'],
                ev.target['data-answer-id']]);

  // A HTTP response from the Question service is received, just extract its body.
  const questionReceivedAction$ = HTTP
    .map(tapAndLog)
    .filter(res$ => res$.request.url.indexOf(QUESTION_SERVICE_URL) === 0)
    .mergeAll()
    .map(res => res.body)

  // We need a new question when either we answer an old one, or when we start
  // a new game.
  const getNewQuestionAction$ = Rx.Observable.merge(
    chooseAnswerAction$, newGameAction$)
    .map(() => QUESTION_SERVICE_URL);

  // Just return an object with all the actions in it.
  return {newGameAction$, chooseAnswerAction$,
          questionReceivedAction$, getNewQuestionAction$};
}

// Immutable data structures for questions and State
const Question = Record({
  questionText: "",
  answers: [],
  chosenAnswer: null,
  correctAnswer: null
});

const State = Record({
  questions: new List(),
  score: 0,
  waitingForQuestion: true
});

// Our model turns each action observable into an observable of state
// transformations (i.e. functions state => state), then merges the
// transformation observable$ and `scan`s them over the starting state.
function model(actions) {

  // For a new game, start with a fresh state.
  const newGame$ = actions.newGameAction$
    .map(() => () => new State());

  // If an answer was chosen, check whether it's correct, update its
  // 'chosenAnswer' property, and update the score.
  const chooseAnswer$ = actions.chooseAnswerAction$
    .map(qa => state => {
      const [q_i, a_i] = qa;
      const correctAnswer = state.get('questions').get(q_i).get('correctAnswer');
      const isCorrect = a_i === correctAnswer;

      return state
        .set('waitingForQuestion', true)
        .updateIn(['score'], score => isCorrect ? score + 1 : score)
        .updateIn(['questions'], questions => (questions
          .update(q_i, q => q.set('chosenAnswer', a_i))));
    });

  // And when we receive a new question, check to make sure we're actually
  // waiting for one, and if so just push it onto the list.
  const questionReceived$ = actions.questionReceivedAction$
    .map(q => state => {
      if (state.waitingForQuestion) {
        return state
          .updateIn(['questions'], qs => qs.push(new Question(q)))
          .set('waitingForQuestion', false);
      } else {
        // It's possible we sent too many AJAX requests (e.g. if someone clicked
        // "new game" a lot of times in a row really quickly), so after we
        // receive the first question, we throw the rest away.
        return state;
      }
    });

  // Merge the streams of state transformations, and accumulate them starting
  // with initialState.
  return Rx.Observable.merge(newGame$, chooseAnswer$, questionReceived$)
    .startWith(new State())
    .scan((state, transform) => transform(state));
}

function renderQuestion(question, questionId) {
  const { questionText, answers, chosenAnswer, correctAnswer } = question;
  const isAnswered = chosenAnswer !== null;
  const isCorrect = isAnswered && chosenAnswer === correctAnswer;

  // Show the questionText and then all the answers.
  return div([
    div(".question", { 'data-question-id' : questionId},
        (questionId + 1) + ". " + questionText),
    ...answers.map((answer, answerId) => {
      let class_ = ".answer";
      if (isAnswered) {
        class_ += (answerId === correctAnswer) ? ".correct" : ".wrong";
        if (answerId === chosenAnswer) { class_ += ".chosen"; }
      } else {
        class_ += ".unanswered";
      }

      return div(class_, {
        'data-question-id' : questionId,
        'data-answer-id' : answerId
      }, answer);
    })
  ])
}

function renderQuestions(questions) {
  return div('.questions',
             questions.toArray().map(renderQuestion));
}

function renderScore(score) {
  return div('.score', "Score: " + score);
}

// Render the state.
function view(state$) {
  return state$.map(state => {
    return div([
      renderQuestions(state.get('questions')),
      div(".right-side", [
        renderScore(state.get('score')),
        button('.new-game', "New Game")
      ])
    ])
  });
}

function main(sources) {
  const actions = intent(sources);
  const state$ = model(actions);
  const vtree$ = view(state$);

  return {
    DOM: vtree$,
    HTTP: actions.getNewQuestionAction$
    //.map(() => QUESTION_SERVICE_({
    //  //url: QUESTION_SERVICE_URL,
    //  //method: 'GET'
    //})).map(tapAndLog)
  };
}

Cycle.run(main, {
  DOM: makeDOMDriver('#app'),
  HTTP: makeHTTPDriver()
});
