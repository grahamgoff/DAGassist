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
d <- ggdag::dagify(
Y ~ X + M + Z,
M ~ X + Z,
C ~ X + Y,
exposure = "X",
outcome = "Y")
# M: mediator / Z: confounder / C: collider

# hypothetical candidate controls
controls <- c("Z", "M", "C")

# Flag controls that would bias the total effect of X on Y:
bad_controls_in(d, controls = c("Z","M","C"), exposure = "X", outcome = "Y")
#> [1] "M" "C"

# expected: c("M", "C")  # mediator & collider are "bad controls"; Z is OK
```
