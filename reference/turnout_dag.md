# DAG behind [turnout_data](https://grahamgoff.com/DAGassist/reference/turnout_data.md)

The graph used to simulate
[turnout_data](https://grahamgoff.com/DAGassist/reference/turnout_data.md),
with `income` marked as the exposure and `turnout` as the outcome. Node
coordinates and display labels are attached, so it plots directly with
[`ggdag::ggdag_status()`](https://r-causal.github.io/ggdag/reference/status.html).
Pass it to
[`DAGassist()`](https://grahamgoff.com/DAGassist/reference/DAGassist.md)
to classify covariates and re-estimate against DAG-derived adjustment
sets.

## Usage

``` r
turnout_dag
```

## Format

A `dagitty` object with 7 nodes and 10 edges.

## Source

Defined in `data-raw/make_data.R`.

## Details

Minimal sufficient adjustment sets, as returned by
[`dagitty::adjustmentSets()`](https://rdrr.io/pkg/dagitty/man/adjustmentSets.html):

- Total effect: `{age, parental_ses}`

- Direct effect: `{age, polint, parental_ses}`

- Canonical (total): `{age, elect_comp, industry, parental_ses}`

## See also

[turnout_data](https://grahamgoff.com/DAGassist/reference/turnout_data.md)
