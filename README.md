# science-questions

The other day the Allen Institute released
<a href = "http://allenai.org/data.html">a dataset of elementary school science questions</a>.
And every dataset is an opportunity to build a stupid data product.

This one is a stupid science quiz generator. It's an end-to-end product with
three phases:

1. Data: extract the questions data and build up a corpus of what science questions
 look like
2. Backend: create an API that uses the corpus to generate and serve random
 (stupid, bogus) science questions.
3. Front-end: create a web-app that consumes the API and allows you to play the
 quiz.

And because I am a glutton for languages, I implemented things multiple times.
There is a Flask version of the backend (easy) and a Haskell version of the backend
(not easy). There is a cycle.js version of the frontend (medium) and a purescript-pux
version of the frontend (not medium).

So bear with me.
