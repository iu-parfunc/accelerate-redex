This repository includes a [PLT Redex](http://redex.racket-lang.org/)
model of the
[Accelerate](https://hackage.haskell.org/package/accelerate) language,
as well as associated fission rules in support of our paper,
"Converting Data-Parallelism to Task-Parallelism by Rewrites."

The model is split into several files. Loading `accelerate.rkt` in
Racket will load all of the semantics and execute the tests.

The `accelerate-core.rkt` file defines the semantics for the core
fragment of Accelerate. This does not include any fissioning rules.

The `fission.rkt` file extends `accelerate-core.rkt` with support for
fissioning. Fissioning splits parallel operators into separate pieces
that can be executed in parallel on different devices. The goal is to
ensure that the fissioned programs evaluate to the same answer as the
original.

The `test-suite.rkt` files defines a number of test cases to ensure
the model matches the desired semantics and that the fission rules do
not introduce errors.

Finally, `render.rkt` is used to generate the figures that are
included in our paper.

The code in this repository is known to work with Racket version
6.1.1. Other versions will probably work as well.
