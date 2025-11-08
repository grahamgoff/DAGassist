# Print method for DAGassist reports

Nicely prints the roles table, highlights potential bad controls, shows
minimal/canonical adjustment sets, optionally shows formulas, and
renders a compact model comparison (using `{modelsummary}` if available,
falling back to `{broom}` or basic
[`coef()`](https://rdrr.io/r/stats/coef.html) preview).

## Usage

``` r
# S3 method for class 'DAGassist_report'
print(x, ...)
```

## Arguments

- x:

  A `"DAGassist_report"` object returned by
  [`DAGassist()`](https://grahamgoff.github.io/DAGassist/reference/DAGassist.md).

- ...:

  Additional arguments (currently unused; present for S3 compatibility).

## Value

Invisibly returns `x`.

## Details

The printer respects the `verbose` flag in the report: when `TRUE`, it
includes formulas and a brief note on variables added by DAG logic
(minimal and canonical sets). Fitting errors are shown inline per model
column and do not abort printing.
