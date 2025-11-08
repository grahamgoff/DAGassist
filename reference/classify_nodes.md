# Classify DAG nodes

Labels each node as one of: `exposure`, `outcome`, `confounder`,
`mediator`, `collider`, `descendant_of_outcome`, or `other`.

## Usage

``` r
classify_nodes(dag, exposure, outcome)
```

## Arguments

- dag:

  A `dagitty` DAG object.

- exposure:

  Optional– inferred from DAG if not set; character; exposure node name
  (X).

- outcome:

  Optional– inferred from DAG if not set; character; outcome node name
  (Y).

## Value

A data.frame with one row per node and columns:

- `variable` (node name)

- logical flags: `is_exposure`, `is_outcome`, `is_confounder`,
  `is_mediator`, `is_collider`, `is_descendant_of_outcome`,
  `is_descendant_of_exposure`

- `role` (a single primary label)

## Details

label definitions *confounder* – ancestor of both X and Y, and not a
descendant of X *mediator* – descendant of X and ancestor of Y
*collider* – node with 2 or more parents on an X / Y path
(non-structural) *descendant_of_outcome* – any descendant of Y
`exposure` / `outcome` labeled explicitly in function call

Notes:

- in definitions, x is exposure and y is outcome

- structural colliders are calculated, but only to define
  non-structural. structural colliders are not included as a boolean
  flag

- A node may satisfy multiple properties; we also return boolean flags
  for each property. The `role` column gives a single "primary" label
  using the precedence defined below.

## Examples

``` r
  d1 <- dagitty::dagitty("dag { Z -> X; Z -> Y; X -> Y }") # confounder Z
  classify_nodes(d1, exposure = "X", outcome = "Y")
#> variable  role        X  Y  conf  med  col  dOut  dMed  dCol  dConfOn  dConfOff  NCT  NCO
#> X         exposure    x                                                                  
#> Y         outcome        x                                                               
#> Z         confounder        x                                                            

  d2 <- dagitty::dagitty("dag { X -> M -> Y }") # mediator M
  classify_nodes(d2, "X", "Y")
#> variable  role       X  Y  conf  med  col  dOut  dMed  dCol  dConfOn  dConfOff  NCT  NCO
#> X         exposure   x                                                                  
#> Y         outcome       x                                                               
#> M         mediator               x                                                      

  d3 <- dagitty::dagitty("dag { X -> C <- Y }") # collider C
  classify_nodes(d3, "X", "Y")
#> variable  role       X  Y  conf  med  col  dOut  dMed  dCol  dConfOn  dConfOff  NCT  NCO
#> X         exposure   x                                                                  
#> Y         outcome       x                                                               
#> C         collider                    x    x                                            
```
