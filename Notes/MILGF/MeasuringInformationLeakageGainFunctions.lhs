% Notes on $g$-Functions
% JMCT
% Nov. 2015

This document implements a toy version of the ideas from "Measuring Information
Leakage using Generalized Gain Functions". But first it introduces the concept
of communications channels.

We begin with some boilerplate

> module GainFunctions where

> import Prelude hiding (lookup)
> import Data.Map as M hiding (map)
> import Data.List.Matrix
> import Data.Maybe (catMaybes)

Information-Theoretic Channels
==============================

We must define some of the concepts we will be using

> type Input  = Int
> type Output = Int

> type Probability = Double
> type Entropy     = Double -- Number of bits

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

Operations on Channels
----------------------

In the paper, they define 'joint distributions' ($p(x,y)$ in the paper) as

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

$$ C = \left| \begin{array}{cccc} 1 & 2 & 3 & 4 \\
                                  5 & 6 & 7 & 8 \\
                                  9 & 10 & 11 & 12
              \end{array} \right| $$

The indices into $C$, $C[x,y]$, represent the probability of seeing $y$ if the
input was $x$. In the paper "Abstract Channels and Their Robust
Information-Leakage Ordering" they use the following example $C$

$$ C = \left| \begin{array}{cccc} 1 & 0    & 0   & 0 \\
                                  0 & .5   & .25 & .25 \\
                                  .5 & .33 & .17 & 0
              \end{array} \right| $$


$C[1,2] = 0.25$ represents the likelihood of seeing $y_{2}$ if the input was
$x_{1}$. In order to take into account any uncertainty about `x` being the
input, we scale the result by the likelihood of `x` being the input:
`prior(x)`.

Now back to the issue of representation. The definition assumes that there is
access to `prior` and `C`. We could embed these values in a Reader Monad, and
that would solve the issue, but instead we'll just explicitly pass these values
into our function. We can always switch to the Reader Monad approach if it
turns out that we're using these values everywhere.

This leaves us with the following definition:

> jointDist :: Prior -> Channel -> Input -> Output -> Probability
> jointDist pi chan i o = pi' * index i o c
>   where
>     (_, _, c) = chan
>     pi' = case lookup i pi of
>             Just v  -> v
>             Nothing -> error "out of bounds in prior lookup"

With `jointDist` defined, we can now define a few other useful functions. The
marginal probability calculates the probability for _any_ output given a specific
input. So we sum the jointDist over each possible output. Written as $p(x)$ in
the paper.

> marginalProbI :: Prior -> Channel -> Input -> Probability
> marginalProbI pi chan i = sum $ map (jointDist pi chan i) [0..y']
>   where
>     (_, y, _) = chan
>     y' = length y - 1

Conditional probability is the quotient of the joint distribution and marginal
probability. Written as $p(y|x)$ in the paper.

> condProbI :: Prior -> Channel -> Output -> Input -> Probability
> condProbI pi chan o i = joint / marginal
>   where
>     joint    = jointDist pi chan i o
>     marginal = marginalProbI pi chan i

The two previous functions also have equivalents for $p(y)$ and $p(x|y)$.

> marginalProbO :: Prior -> Channel -> Output -> Probability
> marginalProbO pi chan o = sum $ zipWith ($) fs $ repeat o
>   where
>     (x, _, _) = chan
>     x' = length x - 1
>     fs = map (jointDist pi chan) [0..x']

> condProbO :: Prior -> Channel -> Input -> Output -> Probability
> condProbO pi chan i o
>   | marginal /= 0 = joint / marginal
>   | otherwise     = error "Conditional probability is undefined when p(y) == 0"
>   where
>     joint    = jointDist pi chan i o
>     marginal = marginalProbO pi chan o

Vulnerability
=============

Given a channel and a prior distribution on the inputs to that channel we can
calculate the _vulnerability_ of that channel. Written as $V(\pi)$ in the
paper.

> vuln :: Channel -> Prior -> Probability
> vuln chan pi = maximum $ catMaybes $ map (flip lookup pi) [0..x']
>   where
>     (x, _, _) = chan
>     x'        = length x - 1

The _posterior vulnerability_ ($V(\pi, C)$, in the paper) is written as

> pVuln :: Prior -> Channel -> Probability
> pVuln pi chan = sum $ map jointD ys
>   where
>     (x, y, c) = chan
>     ys        = [0..length y - 1]
>     xs        = [0..length x - 1]
>     jointD o  = maximum $ map (\x' -> jointDist pi chan x' o) xs

_min-entropy_ is the 'inverse' of vulnerability. Whereas vulnerability is a
_probability_, min-entropy represents the number of _bits of uncertainty_.  To
convert a vulnerability to a min-entrop (written $H_{\infty}(\pi)$ and
$H_{\infty}(\pi, C)$ in the paper) we just take the $log_{2}$ of the
vulnerability and negate it (to make it positive).

> priorMinEntropy :: Channel -> Prior -> Entropy
> priorMinEntropy chan = (* (-1)) . logBase 2 . vuln chan

> postMinEntropy :: Prior -> Channel -> Entropy
> postMinEntropy pi = (* (-1)) . logBase 2 . pVuln pi

Min-entropy Leakage and Capacity
--------------------------------

The last of the main concepts dealing with information channels is _min-entropy
leakage_ and _min-entropy capacity_. When reasoning about a specific prior
min-entropy leakage, `minLeakage`, is a measure of how much $C$ decreases
uncertainty (for an adversary) about a secret. Min-capacity is the worst-case
min-entropy leakage (i.e. universally qualified over all possible priors).

Interestingly there is no standard way to define these concepts. There are
three common choices, which are detailed in [1]. This paper chooses to use a
_relative_ measure for these concepts.  We cannot define min-capacity as it
requires the supernum of a set involving _all_ prior distributions. Instead we
define it in terms of a given set of priors.

> minLeakage :: Prior -> Channel -> Entropy
> minLeakage pi chan = logBase 2 $ pVuln pi chan / vuln chan pi

> minCapacity :: [Prior] -> Channel -> Entropy
> minCapacity pis chan = maximum $ map (flip minLeakage chan) pis


Gain Functions
==============

The problem with analysing systems using only the concepts defined above is
that they are too coarse-grained. In particular the difference between the
`vuln` and `pVuln` assumes that an adversary only benefits by guessing an
_entire_ secret _exactly_ (Section III of the paper). In order to better model
scenarios where adversaries can gain by partially learning a secret or by
knowing that their assumptions are not correct, the authors have introduced
_gain functions_. To quote from the paper directly:

"The idea is that in any such scenario, there will be some set of guesses that
the adversary could make about the secret, and for any guess $w$ and secret
value $x$, there will be some gain that the adversary gets by choosing $w$ when
the secret’s actual value is $x$.  A gain function $g$ will specify this gain
as $g(w, x)$, using scores that range from 0 to 1."

Instead of modelling the set of potential guess as $X$, we allow the guesses to
come from an arbitrary set $W$. The type for $g$-functions in the paper is $g :
W \times X \rightarrow [0,1]$, in Haskell-land a direct translation might be `g
:: (W,X) -> Probability`, but since we aren't concerned with a _particular_
gain function per se, we'll make it a type synonym in order to speak about gain
functions in general. Additionally, they leave the cardinality of $W$ implicit
when passing around $g$-functions, we don't have that luxury and need to pass
it around as well (right now we'll do it was we do with the channels, using the
size of our finite set).

> type W = Vector Input -- Finite set of guesses about the input
> type GainFunction = Input -> Input -> Probability
> type GRep = (W, GainFunction)

We now generalise vulnerability to accept $g$-functions

> gVuln :: Channel -> GRep -> Prior -> Probability
> gVuln chan (gs, g) pi = maximum $ map gMapped [0..g']
>   where
>     gMapped w = sum $ catMaybes $ map (\y -> fmap (* (g w y)) (lookup y pi)) [0..x']
>     (x, _, _) = chan
>     x'        = length x - 1
>     g'        = length gs - 1

The intuition behind `gVuln` is that the vulnerability of our system is bound
by the expected gain to an adversary. If we assume that an adversary's gain is
the maximum amount for any guess (i.e. $g$ always returns 1) then our
vulnerability is actually the _sum_ of our priors, as opposed to the _maximum_
from `vuln` above. This illustrates the fact that if an adversary is able to
learn from _any_ observation, then we are actually more vulnerable than the
standard measure for `vuln` estimates. This is because the standard measure
assumes that the adversary only cares about being _exactly right_ and can't
learn from any other situation, which is not true in general.

Of course, we also have a _posterior_ $g$-vulnerability:

> pGVuln :: GRep -> Prior -> Channel -> Probability
> pGVuln (gs, g) pi chan = sum $ map yMapped [0..y']
>   where
>     yMapped y   = maximum $ map (wMapped y) [0..(length gs - 1)]
>     wMapped y w = sum $ catMaybes $ map (body y w) [0..x']
>     body y w x  = fmap ((* g w x) . (* jointDist pi chan x y)) $ lookup x pi
>     (x, y, c)   = chan
>     (x', y')    = (length x - 1, length y - 1)


Generalise all the things!
--------------------------

For prior and posterior min-entropy we generalise, to $g$-entropy, in the
straightforward way by adding an extra argument for the `GRep`.

> gPriorMinEntropy :: GRep -> Channel -> Prior -> Entropy
> gPriorMinEntropy grep chan = (* (-1)) . logBase 2 . gVuln chan grep

> gPostMinEntropy :: GRep -> Prior -> Channel -> Entropy
> gPostMinEntropy grep pi = (* (-1)) . logBase 2 . pGVuln grep pi


The $g$-function analogues of `minLeakage` and `minCapacity` are also
straightforward. 

$g$-leakage:

> minGLeakage :: GRep -> Prior -> Channel -> Entropy
> minGLeakage grep pi chan = logBase 2 $ pGVuln grep pi chan / gVuln chan grep pi

$g$-capacity:

> minGCapacity :: [Prior] -> GRep -> Channel -> Entropy
> minGCapacity pis grep chan = maximum $ map (flip (minGLeakage grep) chan) pis


Example $g$-Functions
=====================


The simplest _realistic_ $g$-function is the _identity gain function_, where
the adversary gains the maximum amount of information if they guessed
correctly, and gain no information otherwise.

> gId :: GainFunction
> gId w x
>   | w == x    = 1
>   | otherwise = 0

For `gId` above, we assume that $\left| W \right| = \left| X \right|$. Representing it as a matrix, `gId` is just
the identity matrix, $I_{\left|X\right|}$.

We are now able to write our first proposition! (TODO: Make this quickcheckable)

Prop: gVuln gId == vuln


Using Metrics to Create $g$-Functions
-------------------------------------

In many scenarios, it may be possible to reason about the _distance_ between
two inputs. It is possible to generate a $g$-function from any such distance
metric.

Given a function $d : X \times X \rightarrow [0,\infty)$ that computes the distance
between two elements of $X$, it must first satisfy three simple properties:

1. Identity of indescernables: $d(x_{1}, x_{2}) = 0$ iff $x_{1} = x_{2}$
2. Symmetry: $d(x_{1}, x_{2}) = d(x_{2}, x_{1})$
3. Triangle Inequality: $d(x_{1}, x_{3}) \leq d(x_{1}, x_{2}) + d(x_{2}, x_{3})$

If $d$ satisfies the three above properties you can generate a $g$-function by first
normalizing $d$ to create $\overline{d} : X \times X \rightarrow [0,1]$. Then the
$g$-function, $g_{d}$, is defined as

$$g_{d}(w, x) = 1 - \overline{d}(w, x)$$


[1]: "Recent Developments in Quantitative Information Flow" Geoffrey Smith

