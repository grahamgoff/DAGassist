# Balance diagnostics across a list of fitted models

Compares the complete-case analytic sample of a reference model against
each other model in a named list, flagging covariates whose sample
composition shifts (`|(S)MD| > threshold`). Engine-agnostic: it reads
variable names from each model and recomputes samples on `data`, so it
works for `lm`, `glm`, `feols`, `coxph`, weighted fits, etc. Models are
never refit.

## Usage

``` r
balance_models(
  models,
  data,
  reference = 1,
  covariates = NULL,
  threshold = 0.1,
  extra_vars = NULL,
  include_outcome = FALSE
)
```

## Arguments

- models:

  Named list of fitted models. The names label the comparisons.

- data:

  The shared source data frame (a superset of every model's rows).

- reference:

  Name or index of the model that defines the baseline sample.

- covariates:

  Optional character vector of covariates to assess. Default: the union
  of all models' predictors present in `data`.

- threshold:

  Flag covariates with `|(S)MD|` above this (default 0.1).

- extra_vars:

  Additional variables that drive listwise deletion (e.g. a weight or
  cluster column not detected from the call).

- include_outcome:

  Assess the outcome variable too (default FALSE).

## Value

An object of class `DAGassist_balance`: a list with `$reference`,
`$comparisons` (per-comparison (S)MD tables) and `$summary` (one row per
comparison: n_ref, n_cmp, n_covariates, n_flagged, pct_flagged,
any_flagged).

## Examples

``` r
# Rows with missing C drop out of the Original model only
d <- toy_data
d$C[d$Z > 1] <- NA
balance_models(
  list(Original = lm(Y ~ X + M + C + Z, data = d),
       Minimal  = lm(Y ~ X + Z, data = d)),
  data = d
)
#> Balance diagnostics (reference: Original)
#>   |(S)MD| > 0.10 flags a covariate whose sample shifts vs the reference (binary vars use a raw difference).
#>   Original vs Minimal: n = 1698 vs 2000  [!] 3 covariate(s) imbalanced
#>       Z                        (S)MD = -0.348
#>       X                        (S)MD = -0.187
#>       M                        (S)MD = -0.143
```
