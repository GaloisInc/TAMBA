## Abstract

Quantitative information flow is concerned with measuring the knowledge about secret data that is gained by observing the answer to a query. This measurement has important applications in the domain of privacy, as an increase in attacker knowledge corresponds to a decrease in the privacy of a user's data. We present a novel combination of probabilistic abstract interpretation and sampling that can compute high-confidence upper bounds on information flow more accurately and more efficiently than prior methods.  The technique first computes a sound but imprecise upper bound and then uses sampling to refine this bound, delivering a superior accuracy / efficiency tradeoff as compared to either abstract interpretation or sampling in isolation.  Furthermore, we implement a number of heuristics to improve the first phase of this algorithm, which are directly applicable to existing abstract interpretation based methods.  We present detailed experimental results that quantify the improvement due to each of these factors and highlight which sorts of queries benefit most from which techniques.  Combined, these methods allow us to automatically obtain bounds for problems that are computationally infeasible for other approaches.

## Outline

1. Introduction
  - Probabilistic programming useful in general
  - Useful for security in particular
    + Where uncertainty considers adversary knowledge, and programs are queries
    + Requires exact interference for query auditing to be sound
  - Prior works that perform exact inference are limited in terms of precision/performance
    + Mention Prob work in particular, in terms of what it can do
  - This work: Augment exact inference algorithm with dynamic analysis (sampling) to soundly augment the results
    + Sampling, augment (sound) lower bounds
    + Sampling, can apply to lower and upper bounds, but with confidence intervals
      - Superior to either sampling or analysis in isolation
    + Concolic execution, augment (sound) lower bounds
  - Evaluation shows this balances precision and performance quite well
    + Compared to old prob
    + Compared to other exact inference approach (e.g., Hansei)
    + _Not sure: comparison to sampling-only approach, just as a baseline_
2. Related work
  - Sampling-based languages: Not sound, very expressive
  - Languages with exact inference: Sound, but limited in expressiveness and/or precision
3. Overview
  - Use ships example to explain stuff
4. Formalism
  - Not sure what goes here
5. Implementation
  - Some details on what we did
6. Experiments
  - **Need to figure out what applications and experiments to run**
7. Conclusions
