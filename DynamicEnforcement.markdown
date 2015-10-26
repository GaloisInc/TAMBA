% Notes on the paper 'Dynamic Enforcement of Knowledge-based Security Policies'
% JMCT

General Idea
============

There is the well known game '20 Questions'. The idea of the game is that one
person, $A$, thinks of something, $s$, keeping it secret. Another person (or
group of people) $Q$ ask a series of 'yes or no' questions attempting to deduce
what $s$ is.  This game illustrates that $Q$ can infer very specific knowledge
about something $A$ has kept private because $A$ slowly 'leaks' information.
While each question may be innocuous on its own, it is the _combination_ of the
information provided by the answers that allows the secret $s$ to be inferred.

This demonstrates an important fact about privacy and security (stated in two ways):

* Keeping $s$ secret doesn't prevent another party from learning $s$
* Revealing enough non-secret information is equivalent to revealing secret
  information

Because of this, those interested in security and/or privacy tend to enforce
very strict rules about what information can be shared. Often this means that
even benign information is kept secret because it may be used, along with other
bits information, to infer secret information. This tactic forms a rather blunt
instrument. You can imagine that such a policy would prevent the sharing of
information that might be useful _and harmless_ to share, simply because
information that _may_ be shared later would allow another party to infer a
secret.

Imagine instead that we model the possible inferences that could be made from
the information we have already shared. This would allow us to decide, on a
case-by-case basis, what information is safe to share. If we have already
shared data $a$, $b$, and $c$, it may not be safe to share $d$, because with
the new information provided by $d$ the other party would be able to infer some
secret information. On the other hand, it may be safe to share $d$ if we have
not shared any other information. This is the intuition behind _knowledge-based
security policies_, we decide what we can share based on modelling what another
party is able to infer from what we have already shared.

Concrete Example:
-----------------

Often systems will store the following data regarding a user:

1. Birthday, including:
    a. Day
    b. Month
    c. Year
2. zip-code
3. gender

These three items are known to be able to uniquely identify a majority of the
US population. If a user is privacy conscious they may prohibit the application
from sharing any of this information despite the fact that they would be okay with
anyone knowing their birth-year, or zip-code, etc..

The proposed solution to this is to allow the user to specify how difficult
they would like it to be to infer their identity. This allows the application
to share some of this information while ensuring that not enough is shared to
allow a third party to infer the identity of the user. For example, the user
could state that they do not want anyone to be able to narrow down the identity
to fewer than 1,000 people (i.e. a $\frac{1}{1000}$ chance of identifying the
user).

A Little More Formal
--------------------

We have three actors:

* $U$: the user
* $A$: the user's agent
* $Q$: a querying principal, the third part trying to infer information about $U$

This paper introduces _Knowledge-based Security Policies_ which determine how
the user's agent $A$ can safely respond to queries from $Q$ about user $U$. The
technique accomplishes this by having $A$ maintain a probability distribution
over $U$'s secret information for $Q$. $A$ may determine that $Q$ is able to
infer $U$'s birth-year but not their birth-month and birth-day. If by sharing
$U$'s birth-day $Q$ would be able to uniquely identify $U$, then $A$ would not
share that data. However, if sharing the birth-day still keeps the probability
of inferring $U$'s identity below $U$'s specified level, then $A$ has the
option to share that data.

The user can specify a different privacy policy for each $Q$, allowing
flexibility in what is shared with who.

How it's Done
-------------

There are two parts to providing knowledge-based security policies:

1. An algorithm for knowing when a query violates the policy
2. A way to update the model of a $Q$'s beliefs based on responses to queries

One of the key contributions of the paper is providing a method for implementing
both of the above. Because these models are probabilistic in nature the use of
a probabilistic language is a natural choice. However, the standard method, which
uses the sampling of probability distributions, is too inefficient when dealing with
large state spaces (for the purposes of this work), so instead the implementation
is based on _abstract interpretation_.

A Small Hitch
-------------

Unfortunately, we cannot have $A$ simply reject a query when the answer will allow
$Q$ to know $U$'s secret information precisely. The reason for this is because
the rejection of a query _is itself providing information_. The example in
the paper is quite clear:

Imagine $U$'s birthday is stored as a tuple: $(bday, byear)$. $Q$, wanting
to know which users' birthday is in the next week sends this query to all
users:


```c
today = 260;
output = False;
if (bday >= today && bday < (today + 7))
    output = True;
```

In the above code, `today` references a non-secret global variable that
represents the current day in the year. $U$'s secret variable `bday` needs to
be accessed in order to respond to the query with a `True` or `False`. For our
purposes, we will assume that $U$ has specified that no $Q$ should be able to
determine $U$'s birtday to within 5%.

The question we concern ourselves with is: When is it safe to respond to these
kinds of queries?

Let's consider a few possibilities. The first is that $U$'s `bday` value is
270.  This means that the result of running the code above is `False`. What
does that response allow $Q$ to infer? Well, assuming that this is the first
query for $U$, then the $Q$ began with the following information (we use $C$ to
represent the possible knowledge that $Q$ has about $U$, and $m$ to represent
the probability of the knowledge to be true):

$$C = 0 \leq bday < 365; m = 1$$

In plain English: We know that the user's birthday is definitely a day in the
year.

After receiving `False` to the query above, $Q$ now knows that `bday` is _not_
between 260 and 267. This allows $Q$ to split $C$ into two separate estimates:

$$ C_{1} = 0   \leq bday < 260; m_{1} \approx 0.726 $$
$$ C_{2} = 267 \leq bday < 365; m_{2} \approx 0.274 $$

Because the actual value of `bday` is equally likely (from $Q$'s perspective)
to be on any day other than the days of this week, $m_{1}$ and $m_{2}$ are
weighted by the number of days before and after this week, respectively.
