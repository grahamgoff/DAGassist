# Diagnose robustness to added ("missing") edges

Tests how the adjustment set and identification respond to edges the
root DAG assumes are absent (DAGWOOD exclusion branches). Each added
edge is evaluated as its own branch DAG (root + that edge).

## Usage

``` r
add_edges_robustness(dag, exposure, outcome, add_edges, formula = NULL)
```

## Arguments

- dag:

  A `dagitty` DAG.

- exposure, outcome:

  Optional; inferred from the DAG when omitted.

- add_edges:

  Character vector like `c("Z -> Y", "X <-> Y")`. Directed (`->`, `<-`)
  and bidirected (`<->`, latent common cause) edges are supported.

- formula:

  Optional model formula or engine call; used only to decide whether a
  role-flipping covariate is in your specification.

## Value

A `DAGassist_addedge_summary` object.

## Examples

``` r
# What if the DAG is missing an arrow, or has unmeasured confounding?
add_edges_robustness(toy_dag, add_edges = c("A -> X", "X <-> Y"))
#> 
#> Edge-addition (exclusion) robustness:
#> - edges tested: 2
#>   - A -> X: minimal changed: yes; canonical changed: no
#>         new minimal set(s): {A, Z}
#>         role changes: A: nco->confounder
#>   - X <-> Y: effect NOT identifiable if this pathway exists (no adjustment set blocks it)
#> - re-estimation recommended: yes
```
