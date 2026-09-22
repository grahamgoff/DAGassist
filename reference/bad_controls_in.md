# flag bad controls (mediator/collider/desc of Y) among a candidate set

flag bad controls (mediator/collider/desc of Y) among a candidate set

## Usage

``` r
bad_controls_in(dag, controls, exposure, outcome)
```

## Arguments

- dag:

  A `dagitty` DAG object.

- controls:

  Character vector of variable names.

- exposure:

  Character; exposure node name (X).

- outcome:

  Character; outcome node name (Y).

## Value

A character vector (possibly empty) containing the elements of
`controls` that are identified as "bad controls".

This is essentially the inverse of `pick_minimal_controls()`, as it
returns bad controls, rather than the minimal/canonical set of good
controls

## Examples

``` r
# Which variables in a formula are bad controls, given the DAG?
bad_controls_in(toy_dag, Y ~ X + M + C + Z)
#> Error in unique.default(x): unique() applies only to vectors
```
