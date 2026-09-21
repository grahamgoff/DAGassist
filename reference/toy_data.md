# Toy data for demonstrating causal roles

A small simulated dataset in which each causal role that
[`DAGassist()`](https://grahamgoff.com/DAGassist/reference/DAGassist.md)
classifies appears at least once. Generated from
[toy_dag](https://grahamgoff.com/DAGassist/reference/toy_dag.md), so the
correct adjustment set is known by construction: the total effect of `X`
on `Y` requires adjusting for `Z` alone.

## Usage

``` r
toy_data
```

## Format

A data frame with 2,000 rows and 7 columns:

- X:

  Exposure. Caused by `Z`.

- Y:

  Outcome. Caused by `X`, `M`, `Z`, `A`, and `B`.

- Z:

  Confounder: a common cause of `X` and `Y`.

- M:

  Mediator: lies on the path `X -> M -> Y`.

- C:

  Collider: a common descendant of `X` and `Y`.

- A:

  Neutral control on the outcome; affects `Y` only.

- B:

  Neutral control on the outcome; affects `Y` only.

## Source

Simulated by `data-raw/make_data.R`.

## Details

The data-generating process is linear and Gaussian with `set.seed(42)`.
True coefficients are `X -> Y = 0.7`, `M -> Y = 0.6`, `Z -> Y = 0.3`,
`A -> Y = 0.2`, `B -> Y = -0.1`.

## See also

[toy_dag](https://grahamgoff.com/DAGassist/reference/toy_dag.md) for the
generating graph.
