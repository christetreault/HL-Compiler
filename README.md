HL Compliler
============

This project is part of a research project to discover ways to optimize Logic Programming systems such as the the Coq Proof Assistant's Ltac tactics language.

The Ltac language is very powerful, and can be used to prove many useful properties about the correctness of programs. Unfortunately, it is also very slow, which limits its usefulness. If we can improve this state of affiars, it would be of great value to the community.

Current Status
--------------

Currently, we have implemented a compiler that converts our AST into a Haskell program. At the moment, there is no frontend to this compiler, so ASTs must be built manually. See [`Demo.EvenOdd`](src/Demo/EvenOdd.hs), [`Demo.LambdaCalc`](src/Demo/LambdaCalc.hs), and [`Demo.PNCalc`](src/Demo/PNCalc.hs) for examples of how to do this.

These three modules represent demos that we've created to test the correctness of our compiler, and will form the basis of further research into optimizations.

To interact with the system, you should invoke `cabal repl` and manually call `query` or `unifies`  exported by `HL.Query`. The main function of the executable produced by `cabal install` runs the benchmark suite.

Testing
-------

To run the test suite, simply execute:

```
$ cabal test
```

This will invoke the test suite which will test our three demo modules.

Benchmarking
------------

To run the benchmark suite, simply execute:
```
$ cabal install
$ cabal exec hlCompiler
```

This will perform stress tests of our demo modules.
