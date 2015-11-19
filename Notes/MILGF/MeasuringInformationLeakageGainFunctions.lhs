% Notes on $g$-Functions
% JMCT
% Nov. 2015

This document implements a toy version of the ideas from "Measuring Information
Leakage using Generalized Gain Functions". But first it introduces the concept
of communications channels.

We begin with some boilerplate

> module GainFunctions where

> import Prelude hiding (lookup)
> import Data.Map as M
> import Data.List.Matrix

And we must define some of the concepts we will be using

> type Input  = Int
> type Output = Int

> type Probability = Double

> type X = Vector Input         -- The finite set of inputs (indices matter)
> type Y = Vector Output        -- The finite set of outputs (so we must use [])

> type C = Matrix Probability   -- Channel Matrix TODO: improve

There are some invariants on $C$. Namely, each entry in the matrix is between 0
and 1 and the rows of the matrix sum to 1. If all the entries are _either_ 0 or
1 then we say that the channel is deterministic (each row will contain exactly
one 1, so each input produces a unique output).

A 'Channel' combines these together, providing a representation of all possible
inputs and outputs, and the Channel Matrix that maps between them.
Interestingly, we don't actually need to carry around the vectors for $X$ and
$Y$, all we care about is their cardinality so we could just carry around two
`Int`s. For now we're sticking to the 'textbook' definition though in case that
changes.

> type Channel = (X, Y, C)

We must also be able to reason about distributions over the values in a
channel.  In general, a distribution over a set of values is a map from an
element of that set to a probability of that element occurring. As we've got to
make it concrete for the computer, probabilities are represented as `Doubles`.

> type Dist a = Map a Probability

With this as a given, a prior distribution over $X$ is just a distribution over
`Input`s

> type Prior = Dist Input

In the paper, they define 'joint distributions' as

< jointDist :: (Input, Output) -> Probability
< jointDist (x, y) = prior(x) * C(x, y)

Mapping this to Haskell leaves us with a few choices. The trivial choice is
whether to write the function as curried or uncurried. We'll choose curried, of
course. However, the non-trivial decision does not have a clear 'better'
solution. In the paper's definition we see that there is implicit access to a
term-level prior and to a Channel Matrix. The use of prior as a function is the
standard 'lookup' operation in a Map. Let's discuss the channel matrix and
how it works for a second.

Imagine a program with 3 possible inputs. This means that we would have a size
3 vector (in the Mathematics sense) of type `X`:

|X| = 3

For illustration let's say there are four possible outputs, giving us a vector
of size 4 for the outputs.

|Y| = 4

Our channel matrix maps the input vectors to output vectors. So the dimensions
of the matrix are $|X|*|Y|$. In this example that would mean our channel matrix
must be of size 3 * 4. It would look something like this:

C = |1 2 3  4 |
    |4 5 6  7 |
    |8 9 10 11|

The indices into $C$, $C[x,y]$, represent the probability of seeing $y$ if the
input was $x$. In the paper "Abstract Channels and Their Robust
Information-Leakage Ordering" they use the following example $C$

C = |1   0   0    0   |
    |0   .5  .25  .25 |
    |.5  .33 .17  0   |


C[1,2] = 0.25 -> The likelihood of seeing $y_{2}$ if the input was $x_{1}$. In
order to take into account any uncertainty about `x` being the input, we scale
the result by the likelihood of `x` being the input: `prior(x)`.

Now back to the issue of representation. The definition assumes that there is
access to `prior` and `C`. We could embed these values in a Reader Monad, and
that would solve the issue, but instead we'll just explicitly pass these values
into our function. We can always switch to the Reader Monad approach if it
turns out that we're using these values everywhere.

This leaves us with the following definition:

> jointDist :: Prior -> Channel -> Input -> Output -> Probability
> jointDist pi chan i o = pi' * index i o c
>   where
>     (x, y, c) = chan
>     pi' = case lookup i pi of
>             Just v  -> v
>             Nothing -> error "out of bounds in prior lookup"
