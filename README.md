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

![science quiz](https://raw.githubusercontent.com/joelgrus/science-questions/master/science-quiz.png)

The data layer is implemented in python. (python-data)
The backend was originally implemented in Haskell (haskell-servant) but I couldn't figure out how to deploy the binary to EC2, so I also created a Python version (python-flask).
The first version of the frontend was in cycle.js (cycle-js) but for some reason it stopped working. The current version
is implemented in PureScript (purescript-pux).
