Continued Fractions
=========

[![Build Status](https://travis-ci.com/rockbmb/continued-fractions.svg)](https://travis-ci.com/rockbmb/continued-fractions)

`continued-fractions` is a Haskell library for manipulating and evaluating continued
fractions.

To use this library, the following information is relevant:

* The `CF` datatype is defined thusly:
```haskell
data CF a = CF a [a]
          | GCF a [(a,a)]
```
  where the `CF` constructor is used to represent continued fractions whose numerators
  are all `1`, and `GCF` represents generalized continued fractions. These constructors
  are not exported.

* The `cf :: a -> [a] -> CF a` function is used to create continued fractions.

* The `gcf :: a -> [(a,a)] -> CF a` function is used to create generalized continued
  fractions.