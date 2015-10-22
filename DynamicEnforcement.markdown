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
