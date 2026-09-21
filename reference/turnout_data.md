# Simulated data on income and voter turnout

A simulated cross-section used to demonstrate the
[`DAGassist()`](https://grahamgoff.com/DAGassist/reference/DAGassist.md)
workflow on an applied question: does higher income increase voter
turnout? Because the data-generating process is known, the correct
answer is known too, and the cost of a misspecified control set can be
read directly off the output.

## Usage

``` r
turnout_data
```

## Format

A data frame with 5,000 rows and 7 columns:

- turnout:

  Outcome. Voter turnout propensity.

- income:

  Exposure. Caused by `state`, `age`, and `industry`.

- state:

  Confounder: a common cause of `income` and `turnout`.

- age:

  Confounder: affects `income`, `turnout`, and `industry`.

- polint:

  Political interest. A mediator on the path
  `income -> polint -> turnout`. Adjusting for it removes the indirect
  effect and is a bad control for the total effect.

- industry:

  A cause of `income` lying on the back-door path
  `income <- industry <- age -> turnout`. Safe to adjust for, but not
  required once `age` is included.

- elect_comp:

  Election competitiveness. A neutral control on the outcome: it affects
  `turnout` only, so adjusting for it leaves the estimate unbiased and
  improves precision.

## Source

Simulated by `data-raw/make_data.R`.

## Details

Generated from
[turnout_dag](https://grahamgoff.com/DAGassist/reference/turnout_dag.md)
with `set.seed(42)`. Income raises turnout directly (`0.30`) and
indirectly by raising political interest (`0.50 * 0.40 = 0.20`), so the
**true total effect is `0.50`** and the **true direct effect is
`0.30`**.

Adjusting for `{age, state}` recovers the total effect; adjusting for
`{age, polint, state}` recovers the direct effect. Regressing `turnout`
on every available covariate returns roughly `0.28` — close to the
direct effect, because conditioning on the mediator `polint` silently
changes the estimand rather than producing an obviously wrong number.

## See also

[turnout_dag](https://grahamgoff.com/DAGassist/reference/turnout_dag.md)
for the generating graph;
[toy_data](https://grahamgoff.com/DAGassist/reference/toy_data.md) for a
smaller example covering every causal role.
