# CLTRACE

> [!WARNING]
> This software is *ALPHA* quality at the moment, use at your own risk

`CLTRACE` is a Common Lisp observability framework to debug live production
systems with minimum interference.

It takes inspiration from [`dtrace`](https://en.wikipedia.org/wiki/DTrace) in
order to make debugging programmable and interactive.

You can both deploy debugging programs as well as create them interactively
on-the-fly.

## Usage

``` common-lisp
;; This will register a probe on entry on the cltrace:some-test function
;; when the argument a is 3 we increment the counter `counts` for key `fn-name`.
;; This allows us to keep a cheap aggregation across probes. In this case counting
;; the number of calls to each function we are doing over the lifetime of the program
;;
;; We then use `setvar` to set the value of a program-scoped variable that can then be
;; used in other probes.
(cltrace:define-program some-name
  ((:entry cltrace:some-test :when (= a 3))
   (agg! counts fn-name :count)
   (setvar :value t)))
```

## LLM Usage

LLMs didn't write a single line of this code, however I did use them as
reference and for brainstorming.
