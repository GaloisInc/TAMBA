% Talk on Privacy-Preserving Deep Learning
% Reza Shokri - University of Texas, Austin
% Nov. 3rd 2015

Talk
====


How to use deep learning in a privacy preserving manner.

Deep learning acheives good results on image/speech recognition type tasks.

Used for all sorts of services: Google, MS, Apple, Amazon, etc. use these
technologies to great effect. 

This is because of the massive amounts of data they have to train these nueral
networks.

Also, new GPU tech helps accelerate the processing.


Privacy Concern:
----------------

* The data used for training could be sensitive
* Users don't have control over data or the objective of the training
* When the data is uploaded and the model is constructed users must share data
  to use the resulting system

Consequences:
-------------

* User's data could be used in wrong context
* User's that are not willing to share data can not take advantage of the tech

Objective:
----------

Completely get rid of the could setting of this tech.

Let users keep control of their data. 

Build an equivalent NN as the one that would have been built with publicly
shared data.


Review of NN
------------

Many layers of nodes, each node computes a function on its input and passes the
result on to the next layer

Each layer has parameters (weights), these paramteres are trained using
stochastic gradient descent


1. feed the network with training data
2. compute the error of the output of the network
3. back-propogate through the network
  a. calculate the contribution of each node to the error
  b. calculate a DEQ that represents the system
4. Minimise the DiffEQ (parameter update)
5. Repeat for each batch of training data

(The entire process is called 'one epoch of training') We can perform many
epochs of training



Prior Work:
-------

Secure multi-party computation

Data-sets are too big for this approach (efficiency problems).

instead
-------

Each data holder retains their data, each entity trains on its local private
data.

Problem -> you over-fit on your local data.

Solution -> Minimal information exchange

With this you can assure an accurate model (locally)

This is called DSSGD (Distributed Selective Stochastic Gradient Descent)


Selective SGD
-------------

Select a small subest of the nodes from step 3a. and share those gradients with
the other users

The gradient values (how much a parameter much change) allow the users to avoid
over fitting, because the large gradient changes are the ones that correct
overfitting.


Architecture
------------

Local training on local data set

you share some gradients with others

download latest gradients from the ones upload by other users

Results
-------

Only local does much worse than centralised

DSSGD does a little worse that centralised, but does converge to as the
collaborative training takes place

Privacy Properties
------------------

* Participanst data remains pricate
* Full control over parameter selection
* known learnign objective
* Resulting model available to all parties

Indirect Information Leakage (through gradient sharing)
-------------------------------------------------------

They don't know how this attack would work, but it may be possible:

Use differentially private parameter selections and gradient sharing

Perturb to-be-shared gradient values

### Sparse Vector Technique

* Select a small fraction of (perturbed) gradients that are above a given
  (perturbed) threshold

Conclusion
==========

* DiffPriv exchange protocol prevents indirect leakage about private datasets
* System is substantially better than that of standalone learning and is very
  close to the accuracy of centralised SGD


Discussion
==========

Q: If you send _all_ the parameters, isn't that the same as sharing the data?

A: No. And we don't share all parameters. And users have full control over
  parameter selection, if some parameters allow others to infer local data, those
  parameters can remain private.

Q. How is privacy budget mapped back to data
A: It's fuzzy, because of DiffPriv we can't know that without actually having an attack.

Q: Then why we I like DiffPriv
A: I don't like DiffPriv
A: No other way to guarantee things...

Q: For this setup, the data, in a sense, is equivalent to the gradients in that
  all you need to know are the gradients. So different tasks may have gradients
  that leak more information. What can you say about how much information do the
  gradients leak
A: This is something they envision working on. Before that we want to know that
  if we have a model that is trained, how much information is leaked by _that_. 

Q: For each participant, if they have a random sample over the training data,
  so that each participant had the same number of samples for each training set.
  Thoughts?
A: Here the assumption is that the architectures for each model is the same
  across the participants. Users agree on the optimisation function etc. then
  they train. The question assumes a slightly different architecture (maybe you
  don't have positive samples for a class, for example), this adds noise. Their
  technique does better when its different data, but they have samples for each
  class.

Q: Compare to Ensemble algorithms
A: Ensemble algorithms do all theri training locally, _then_ discuss amongst
  them. This avoids over-fitting
Q: But the majority vote of the ensemble will avoid that over fitting
A: That leaks some data, because of having to share some samples to vote on

Q: Why did you choose NN?
A: There are lots deep learning algorithms being used on sensitive data, and NN
  are the most popoular technique for deep learning because none of the standard
  ML models can beat this on pattern recognition tasks



Reza likes the selective sharing of parameters and gradients.

