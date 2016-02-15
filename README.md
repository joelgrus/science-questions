# science-questions

The other day the Allen Institute released
<a href = "http://allenai.org/data.html">a dataset of elementary school science questions</a>.
And every dataset is an opportunity to build a stupid data product.

This one is a stupid science quiz generator. It's an end-to-end product with
three phases:


1. Data: extract the questions data and build up a corpus of what science questions look like (http://joelgrus.com/2016/02/15/building-a-stupid-data-product-part-1-the-data-python/)

2. Backend: create an API that uses the corpus to generate and serve random
 (stupid, bogus) science questions (http://joelgrus.com/2016/02/15/building-a-stupid-data-product-part-2-the-web-service-haskell/)

3. Front-end: create a web-app that consumes the API and allows you to play the
 quiz (http://joelgrus.com/2016/02/15/building-a-stupid-data-product-part-3-the-single-page-app-purescript/)

![science quiz](https://raw.githubusercontent.com/joelgrus/science-questions/master/science-quiz.png)

For now there's a version running at http://joelgrus.com/experiments/science-questions/

The data layer is implemented in python. (python-data)

The backend was originally implemented in Haskell (haskell-servant) but I couldn't figure out how to deploy the binary to EC2, so I also created a Python version (python-flask).

The first version of the frontend was in cycle.js (cycle-js) but for some reason it stopped working. 

The current version of the frontend is implemented in PureScript (purescript-pux).
