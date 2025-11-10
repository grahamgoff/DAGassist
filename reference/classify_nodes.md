# Classify DAG nodes

Labels each node by causal role in a console tabular grid. This function
is mostly used as an internal helper, but can be used on its own. Users
are encouraged to alternatively use `DAGassist::DAGassist(show=roles)`
for role table specific output.

## Usage

``` r
classify_nodes(dag, exposure, outcome)
```

## Arguments

- dag:

  A `dagitty` DAG object.

- exposure:

  Optional– inferred from DAG if not set; character; exposure node name
  (Exp.).

- outcome:

  Optional– inferred from DAG if not set; character; outcome node name
  (Out.).

## Value

A data.frame with one row per node and columns:

- `variable` (node name)

- logical flags: `is_exposure`, `is_outcome`, `is_confounder`,
  `is_mediator`, `is_collider`, `is_neutral_on_treatment`,
  `is_neutral_on_outcome`, `is_descendant_of_mediator`,
  `is_descendant_of_collider`, `is_descendant_of_confounder_on_bdp`,
  `is_descendant_of_confounder_off_bdp`

- `role` (a single primary label)

## Note

Roles legend: `Exp.` = exposure `Out.` = outcome `CON` = confounder
`MED` = mediator `COL` = collider `dOut` = descendant of Out. `dMed` =
descendant of any mediator, `dCol` = descendant of any collider
`dConfOn` = descendant of a confounder on a back-door path `dConfOff` =
descendant of a confounder off a back-door path `NCT` = neutral control
on treatment `NCO` = neutral control on outcome

## Examples

``` r
  d1 <- dagitty::dagitty("dag {X[exposure];Y[outcome] Z -> X; Z -> Y; X -> Y }") 
  classify_nodes(d1)
#> variable  role        Exp.  Out.  conf  med  col  dOut  dMed  dCol  dConfOn  dConfOff  NCT  NCO
#> X         exposure    x                                                                        
#> Y         outcome           x                                                                  
#> Z         confounder              x                                                            
  
```
