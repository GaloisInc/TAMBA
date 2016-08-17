% Worked Example of Posterior Vulnerability
% JMCT + SM

Definitons
==========

We use $X$ and $Y$ to represent our input and output spaces, respectively. And
$|X|$, $|Y|$ to represent the number of possible values for those spaces. $x$
and $y$ are used to range over possible values from the input and output
spaces.

Joint Probability
-----------------

$$ p(x,y) = \pi[x]C[x,y] $$

Where $\pi$ is the prior distribution and $C$ is the channel matrix for our
query.


Marginal Probability
--------------------

$$ p_{X}(x) = \sum_{y} p(x,y) $$

and

$$ p_{Y}(y) = \sum_{x} p(x,y) $$


Conditonal Probabilitiy
-----------------------

$$ p(y|x) = \frac{p(x,y)}{p_{X}(x)} $$

and

$$ p(x|y) = \frac{p(x,y)}{p_{Y}(y)} $$

Posterior Vulnerability
-----------------------

$$ V_{p}(\pi, C) = \sum_{y}\ p_{Y}(y)\ \max_{x}\ p(x|y) $$

Two Examples
============

We can not run through a complete example of a simply query.

Our Prior
---------

We are interested in the _uniform_ prior as this provides and upper bound on
the vulnerability. This means that we can actuallly define our prior as
$\pi(\_) = \frac{1}{|X|}$

Our Channel #1
--------------

We represent our query as the following channel matrix:

$$ C_{1} = \left| \begin{array}{cc} 1 & 0 \\
                                1 & 0 \\
                                1 & 0 \\
                                1 & 0 \\
                                0 & 1 \\
                                0 & 1 \\
                                0 & 1 \\
                                1 & 0
              \end{array} \right | $$


If we say that $X$ represents location and $Y$ represents $\{True, False\}$, we
can view this as a boolean query about the location of some entity over some
range. Specifically the query returns $True$ if the entitity is between
locations $4$ and $6$.

With this channel in mind our $\pi$, can now be fully defined: $\pi_{C} =
\frac{1}{8}$


$$ V_{p}(\pi_{C}, C_{1}) = \sum_{y}\ p_{Y}(y)\ \max_{x}\ p(x|y) $$

Using our definition for marginal probability we know that $p(0) = \frac{5}{8}$
and $p(1) = \frac{3}{8}$. So we can expand our sum.

$$ V_{p}(\pi_{C}, C_{1}) = \frac{5}{8}\max_{x}\ p(x|0)\ +\ \frac{3}{8}\max_{x}\ p(x|1) $$

Using the definition for $p(x|y)$ we can see (we show only the calculations for
the maximum values):

$$ p(0|0) = \frac{p(0,0)}{p_{Y}(0)} = \frac{\pi_{C}C_{1}[0,0]}{\sum_{x}\pi_{C}C_{1}[x,0]} = \frac{\ \frac{1}{8}\ }{\frac{5}{8}} = \frac{1}{5}$$

and

$$ p(4|1) = \frac{p(4,1)}{p_{Y}(1)}
          = \frac{\pi_{C}C_{1}[4,1]}{\sum_{x}\pi_{C}C_{1}[x,1]}
          = \frac{\ \frac{1}{8}\ }{\frac{3}{8}}
          = \frac{1}{3}$$

Incorporating this into our calculation of $V_{p}$ gives us


$$ V_{p}(\pi_{C}, C_{2}) = \frac{5}{8}\cdot\frac{1}{5}\ +\ \frac{3}{8}\cdot\frac{1}{3}
                     = \frac{1}{4} $$


Our Channel #2
--------------

Now we can look at another channel for similar query. This time we only output
$1$ on a single input. Our intuition tells us this should leak more, as when we
see the output $1$, we definitively know the input.

$$ C_{2} = \left| \begin{array}{cc} 1 & 0 \\
                                1 & 0 \\
                                1 & 0 \\
                                1 & 0 \\
                                1 & 0 \\
                                0 & 1 \\
                                0 & 1 \\
                                1 & 0
              \end{array} \right | $$

Performing the same calculations as above (eliding some details) gives us

$$ V_{p}(\pi_{C}, C_{2}) = \frac{3}{4}\max_{x}\ p(x|0)\ +\ \frac{1}{4}\max_{x}\ p(x|1)
                     = \frac{3}{4}\cdot\frac{1}{6}\ +\ \frac{1}{4}\cdot\frac{1}{2}
                     = \frac{1}{4} $$

Conclusions
===========

It turns out that this is a known property of this metric. The two contributing
factors for why it will always be equal is that we are using uniform priors and
deterministic channels.

There are a few hints in the literature that this is the case (though no one
seems to say it outright). One hint is in _Measuring Information Leakage using
Generalized Gain Functions_ where they state "... making [the posterior
vulnerability] the (weighted) average of the vulnerabilities of the posterior
distributions". By averaging out the posteriors (the columns), you're bound to
get the same answers in a deterministic channel with a uniform prior.

Another (very big hint) can be found in the introduction of _On the information
leakage of differentially-private mechanisms_ (in a paragraph titled
"Quantitative information flow vs. differential privacy.") where the authors
state "One main difference between the two notions is that quantitative
information flow is an average measure, defined in terms of the expected value
of the probability of a successful attack with respect to all possible
outcomes, while differential privacy is concerned with the worst-case, no
matter how improbable it may be."


So the question becomes, is this the metric we are after? In the paper _Dynamic
Enforcement of Knowledge-based Security Policies using Probabilistic Abstract
Interpretation_ The authors use a slightly different metric which is equivalent
to the following change from

$$ V_{p}(\pi, C) = \sum_{y}\ p_{Y}(y)\ \max_{x}\ p(x|y) $$

to

$$ V'_{p}(\pi, C) = \max_{y}\ \max_{x}\ p(x|y) $$

Doing so for the two channels above would result in

$$ V'_{p}(\pi_{C},C_{1}) = \frac{1}{3} $$

and 

$$ V'_{p}(\pi_{C},C_{2}) = \frac{1}{2} $$

Which does align with our intuitions (the second query leaks more).  Perhaps
what we've shown above is why this other metric was used.
