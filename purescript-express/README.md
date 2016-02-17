Just a *third* implementation of the "random questions" back-end service,
this time using <a href = "https://github.com/dancingrobot84/purescript-express">purescript-express</a>.

It's pretty similar to the <a href = "https://github.com/joelgrus/science-questions/tree/master/haskell-servant">Haskell version</a>

(http://joelgrus.com/2016/02/15/building-a-stupid-data-product-part-2-the-web-service-haskell/)

with the principal differences being

* uses `express` instead of `servant`
* uses `Eff` instead of `IO`
* the "unfold" logic in Haskell was based on recursion into the tail of a `(:)`,
  which is not a very PureScript-y / JavaScript-array-y way of doing things. I
  had to rewrite it with a `loop` helper function that uses `snoc` to accumulate
  results.

I also ran into some bower issues with different libraries needing different
incompatible versions of `purescript-node-*` libraries. I wish I'd kept better
track of how I fixed them. :P

It runs really fast, good for you Node.js!

```bash
$ time curl http://localhost:8080/question
{"questionText":"A statue was impressed with long time?","answers":["Cooling causes evaporation","donate a canyon","Fan","hail"],"correctAnswer":3}
real	0m0.009s
user	0m0.000s
sys	0m0.005s
```
