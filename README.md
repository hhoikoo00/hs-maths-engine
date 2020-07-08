# hs-maths-engine

A Haskell library that defines custom mathematical expressions and performs calculus and linear algebra calculations.

## Explanation

`Expressions.hs` contains definitions for an algebraic expression, able to represent constants and variables, as well as unary and binary operations

`Calculus.hs` contains the following functions implementing calculus operations
* `diff`: calculates the derivative of an expression
* `maclaurin`: calculates the nth sum of the value of an expressions's maclaurin series expansion
* `taylor`: generalisatoin of `maclaurin` function for any given center

`Tests.hs` contains definitions for example expressions and test cases for different functions

## Credit

This library builds up on one of the Haskell lab exercises ("Haskell Calculus") offered to first year Computing students at Imperial College London. Some operations are from the lab exercise, while other operations are written from scratch by me.
