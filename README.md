# CLTRACE

> [!WARNING]
> This software is beta quality at the moment, use at your own risk

`CLTRACE` is a Common Lisp observability framework to debug live production
systems with minimum interference.

It takes inspiration from [`dtrace`](https://en.wikipedia.org/wiki/DTrace) in
order to make debugging programmable and interactive.

You can both deploy debugging programs as well as create them interactively
on-the-fly.

## Usage

``` common-lisp
(cltrace:define-program some-name
  ((:entry cltrace:some-test :when (= a 3))
   (agg! counts fn-name :count)
   (setvar :value t)))
```

## LLM Usage

LLMs didn't write a single line of this code, however I did use them as
reference and for brainstorming.
