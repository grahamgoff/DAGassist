# Counterfactuals and Relevant Terms

``` r
library(DAGassist)
```

## Introduction

Questions of causality predominate throughout the natural and social
sciences ([Pearl 2009](#ref-PearlOverview2009)). However, observational
data are limited in their ability to demonstrate causality because “…one
can never observe the potential outcome under the treatment state for
those observed in the control state, and one can never observe the
potential outcome under the control state for those observed in the
treatment state” ([Morgan and Winship 2015,
45](#ref-MorganWinship2015)). Holland ([1986](#ref-Holland1986))
describes this as the fundamental problem of causal inference. The
counterfactual model provides approaches for inferring causal
relationships from observational data. This article provides a concise
explanation of its ideas and applications.

## The Counterfactual Model

(what is the counterfactual model? what are its basic principals?
focused on the aspects which are encoded in DAGs)

The back-door adjustment criterion has two main components:

- \`\`…conditioning on variables that lie on back-door paths can be an
  effective strategy to identify a causal effect’’ ([Morgan and Winship
  2015, 116](#ref-MorganWinship2015)).
- \`\`…if a set of conditioning variables blocks all back-door paths,
  the analyst must then verify that no variables within the conditioning
  set block the causal effect of interest or otherwise mistakenly adjust
  it away’’ ([Morgan and Winship 2015, 117](#ref-MorganWinship2015)).

Essentially, researchers should condition on only the set of variables
that enables them to close back-door paths.

SUTVA: “SUTVA is simply the a priori assumption that the value of Y for
unit $u$ when exposed to treatment $t$ will be the same no matter what
mechanism is used to assign treatment $t$ to unit $u$ and no matter what
treatments the other units receive” ([Morgan and Winship 2015,
48](#ref-MorganWinship2015)).

On ceteris paribus assumptions: “When a facile ceteris paribus
assumption is invoked to relieve the analyst from having to discuss
other contrasts that are nearly certain to occur at the same time, the
posited causal states may be open to the charge that they are too
improbable or ill-defined to justify the pursuit of a causal analysis
based on a causal analysis based on them” ([Morgan and Winship 2015,
43](#ref-MorganWinship2015)).

## Misapplications of Counterfactuals

(look at kitchen sink regressions and table 2 fallacies) **Covariate
adjustment** is a common method for inferring causality from
observational data. Many empirical practitioners use covariate
adjustment to reduce variance in the outcome of interest, under the
assumption that larger adjustment sets will increase precision. An
extensive and growing body of literature identifies the shortcomings of
this approach. Indiscriminate adjustment may lead to *collider bias* and
the improper representation and causal interpretation of multivariate
models’ control variables lead to the *table 2 fallacy*. The following
subsection explains those pitfalls.

## Causal Graphs

(what are DAGs? define elements.) The Basic Elements of Causal Graphs:

- “Causal effects are represented by directed edges → (i.e.,
  single-headed arrows), such that an edge from one node to another
  signifies that the variable at the origin of the directed edge causes
  the variable at the terminus. These ‘directed’ edges are what give
  graphs composed of nodes and single-headed arrows the general label of
  ‘directed graphs’” ([Morgan and Winship 2015,
  79–80](#ref-MorganWinship2015)).
- “A *path* in any sequence of edges pointing in any direction that
  connects one variable to another’’ ([Morgan and Winship 2015,
  80](#ref-MorganWinship2015)).
- “A *directed path* is a path in which all edges point in the same
  direction” ([Morgan and Winship 2015, 80](#ref-MorganWinship2015)).
- “A variable is a *descendant* of another variable if it can be reached
  by a direct path” ([Morgan and Winship 2015,
  80](#ref-MorganWinship2015)).
- “Most importantly, for directed paths of length one, as in A→B, the
  variable $A$ is the *parent* while the variable $B$ is the *child*”
  ([Morgan and Winship 2015, 80](#ref-MorganWinship2015)).

## 

Holland, Paul W. 1986. “Statistics and Causal Inference.” *Journal of
the American Statistical Association* 81: 945–60.
<https://doi.org/10.2307/2289069>.

Morgan, Stephen L., and Christopher Winship. 2015. *Counterfactuals and
Causal Inference: Methods and Principles for Social Research*. Cambridge
University Press.

Pearl, Judea. 2009. “Causal Inference in Statistics: An Overview.”
*Statistics Surveys* 3: 96–146. <https://doi.org/10.1214/09-SS057>.
