# Diagnose adjustment-set and role robustness to uncertain edge directions

Given a DAG plus one or more edges whose direction is uncertain (a
PDAG), enumerate every acyclic orientation and report whether the
minimal/canonical adjustment sets or covariate roles change – i.e.,
whether your estimand is robust to that structural uncertainty.

## Usage

``` r
pdag_robustness(
  dag,
  exposure,
  outcome,
  uncertain_edges = NULL,
  pdag = NULL,
  formula = NULL,
  max_uncertain = 10L
)
```

## Arguments

- dag:

  A `dagitty` DAG (the "root" model).

- exposure, outcome:

  Optional; inferred from the DAG when omitted.

- uncertain_edges:

  Character vector like `c("A -- B")` naming edges whose direction is
  unknown.

- pdag:

  Optional `dagitty` PDAG; its `--` edges are treated as uncertain.

- formula:

  Optional model formula; used to decide whether an ambiguous covariate
  is actually in your specification (affects re-estimation advice).

- max_uncertain:

  Integer guard on the number of uncertain edges (default 10 -\> up to
  1024 worlds).

## Value

A `DAGassist_pdag_summary` object (printed as a bullet summary).

## Examples

``` r
g <- dagitty::dagitty("dag { Z->X; X->Y; Z->Y; A->B; B->Y }")
dagitty::exposures(g) <- "X"; dagitty::outcomes(g) <- "Y"
pdag_robustness(g, uncertain_edges = "A -- B")
#> 
#> PDAG robustness summary:
#> - uncertain edges specified: 1
#> - worlds evaluated (acyclic orientations): 2
#> - minimal adjustment set changed: no
#> - canonical adjustment set changed: yes
#> - covariate role changed: nco -> ambiguous (nco / other) for A
#> - re-estimation recommended: yes
```
