% Notes on QIF for Dynamic Secrets
% JMCT
% August 3rd 2016

Main Idea
=========

Problem
-------

QIF models usually concern themselves with static secrets: There is some data
$X$ and a query $Q$, how much does answering $Q$ tell an adversary about $X$?

The issue is that in more practical scenarios, the secrets change! Passwords
are reset, keys are expired and renewed, medical records change etc.

Obviously, leaking the current value of the secret is still undesireable, but
we can't stop caring about past values either. If an adversary were to learn
enough information about past values to learn _how_ the secrets change, they
may be able to predict the value of future secrets (a ship that is moving for
example, if you leak enought to infer a vector, you might be able to predict
where the ship will be in the future).

Solution
--------

The authors present a model that is based on _probabilistic automata_ for
program execution and _strategy functions_ for the change in dynamic secrets
themselves.

In their proposed model the adversaries are _wait-adaptive adversaries_ which
can observe a system but choose to attack when it benefits them most.

The also propose a metric for quantifying the information flow of dynamic
secrets.

Quantitative Information Flow
=============================

The authors review a bit about QIF for static secrets.

Then they pose the problem of dealing with dynamic secrets in those terms.

Toward QIF for dynamic secrets
------------------------------

The classic model for QIF is insufficient for reasoning about dynamic secrets
in several ways:

* Interactivity: adversary should be able to choose inputs based on past
                 observation
* Input vs. attack: in traditional QIF each input by the adversary is an
                 attack, in an interactive system some inputs may not be
* Delayed attack: the above two features combined would allow for an adversary
                 to delay an attack, possibly waiting for what is perceived
                 to be the optimal time
* Moving target: the traditional QIF framework is not able to handle secrets
                 that are completely replaced by their new value
                    
Model of Dynamic Secrets
========================

The model the authors introduce describes a _system_, this _system_ executes
with a global _context_. This _context_ represents both the secret values and
the adversaries non-secret inputs. During a specific _execution_ the context
and the _system_ interact.

Systems as fully probabilistic automata
---------------------------------------


