# Getting the Most out of DAGassist Using Parameters

## Introduction

[`DAGassist()`](https://grahamgoff.github.io/DAGassist/reference/DAGassist.md)
is meant to be simple and easy to use, and most of its features can be
enjoyed via a simple two-parameter argument:

``` r
library(DAGassist)
library(dagitty)

DAGassist(
  dag = your_dag_model,
  formula = your_regression_call
)
```

But it also offers several parameters for more specific applications.
They control how the DAG is evaluated (`imply`, `eval_all`), how results
print (`show`, `labels`, `omit_factors`, `omit_intercept`, `verbose`),
which modeling engine to use (`engine`, `engine_args`), and which output
format to write (`type`, `out`). This vignette walks through each with
small examples.

## Core Arguments

### `dag` and `formula`

`formula` can be a standard `formula + data` regression call, from which
`DAGassist` will impute the necessary information, or three separate
`formula`, `data`, and `engine` arguments.

``` r
#imputed formula
DAGassist(
  #implies the exposure and outcome from the dagitty object
  dag = dag_model, 
  #implies the engine, formula, and data from the regression call
  formula = lm(Y ~ X + C, data=df) 
)

#plain formula
DAGassist(
  dag = dag_model,
  engine = stats::lm, #stats::lm is the default engine arg
  formula = Y ~ X + C,
  data = df,
  exposure = "X",
  outcome = "Y"
)
```

The two formulas above will print identical output.

## Scope Flags

### `imply`: evaluate on only mentioned variables vs the full DAG

- `imply = FALSE` (default): prune the DAG to just exposure, outcome,
  and your RHS variables; roles/sets are computed on this pruned graph.
- `imply = TRUE`: evaluate on the full DAG and allow DAG-implied
  controls to enter minimal/canonical sets (you’ll be told what’s
  added).

``` r
#pruned-to-formula DAG
DAGassist(dag = dag_model, formula = Y ~ X + C, data = df, imply = FALSE, show = "roles")
#> DAGassist Report: 
#> 
#> Roles:
#> variable  role       Exp.  Out.  conf  med  col  dOut  dMed  dCol  dConfOn  dConfOff  NCT  NCO
#> X         exposure   x                                                                        
#> Y         outcome          x                                                                  
#> C         collider                          x    x                                            
#> 
#>  (!) Bad controls in your formula: {C}
#> 
#> Roles legend: Exp. = exposure; Out. = outcome; CON = confounder; MED = mediator; COL = collider; dOut = descendant of outcome; dMed  = descendant of mediator; dCol = descendant of collider; dConfOn = descendant of a confounder on a back-door path; dConfOff = descendant of a confounder off a back-door path; NCT = neutral control on treatment; NCO = neutral control on outcome

#full-DAG evaluation
DAGassist(dag = dag_model, formula = Y ~ X + C, data = df, imply = TRUE,  show = "roles")
#> DAGassist Report: 
#> 
#> Roles:
#> variable  role        Exp.  Out.  conf  med  col  dOut  dMed  dCol  dConfOn  dConfOff  NCT  NCO
#> X         exposure    x                                                                        
#> Y         outcome           x                                                                  
#> Z         confounder              x                                                            
#> M         mediator                      x                                                      
#> C         collider                           x    x     x                                      
#> A         nco                                                                               x  
#> B         nco                                                                               x  
#> 
#>  (!) Bad controls in your formula: {C}
#> 
#> Roles legend: Exp. = exposure; Out. = outcome; CON = confounder; MED = mediator; COL = collider; dOut = descendant of outcome; dMed  = descendant of mediator; dCol = descendant of collider; dConfOn = descendant of a confounder on a back-door path; dConfOff = descendant of a confounder off a back-door path; NCT = neutral control on treatment; NCO = neutral control on outcome
```

### `eval_all`: keep non-DAG RHS terms in derived models

Sometimes your RHS has terms that aren’t DAG nodes (e.g., fixed effects
via `i(region)`, factor expansions, interactions, splines). `eval_all`
decides whether these non-DAG terms are kept in minimal/canonical
formulas. - eval_all = FALSE (default): drop RHS terms not present as
DAG nodes from the derived formulas. - eval_all = TRUE: keep all
original RHS terms that aren’t DAG nodes (e.g., fixed effects), in
addition to the DAG-based controls.

``` r
DAGassist(
    dag = dag_model,
    formula = fixest::feols(Y ~ X + C + fixest::i(region), data = df),
    imply = TRUE,
    eval_all = TRUE
    )
```

## Display and Labeling

### `show`: sub-reports

- “all” (default): roles grid + model comparison
- “roles”: just the roles/flags table
- “models”: just the model comparison

``` r
# just the roles table
DAGassist(dag = dag_model, formula = Y ~ X + Z + C, data = df, show = "roles")
#just the model comparison
DAGassist(dag = dag_model, formula = Y ~ X + Z + C, data = df, show = "models")
```

### `labels`: human-readable names

Provide a named character vector or a small data frame. Note that the
`label` parameter uses `modelsummary()` `coef_rename` logic, so an
incomplete label list will not throw any errors.

``` r
labs <- list(
  X = "Exposure",
  Y = "Outcome",
  C = "Collider"
)

DAGassist(
  dag = dag_model, formula = lm(Y ~ X + C, data = df),
  show = "roles", labels = labs
)
#> DAGassist Report: 
#> 
#> Roles:
#> variable  role       Exp.  Out.  conf  med  col  dOut  dMed  dCol  dConfOn  dConfOff  NCT  NCO
#> Exposure  exposure   x                                                                        
#> Outcome   outcome          x                                                                  
#> Collider  collider                          x    x                                            
#> 
#>  (!) Bad controls in your formula: {C}
#> 
#> Roles legend: Exp. = exposure; Out. = outcome; CON = confounder; MED = mediator; COL = collider; dOut = descendant of outcome; dMed  = descendant of mediator; dCol = descendant of collider; dConfOn = descendant of a confounder on a back-door path; dConfOff = descendant of a confounder off a back-door path; NCT = neutral control on treatment; NCO = neutral control on outcome
```

### `omit_intercept` and `omit_factors`: output-only filters

These flags only suppress rows in the printed model comparison. They do
not remove terms from estimation. `omit_factors` in particular is useful
for conserving space in your report, as reports with factors included
can be hundreds of rows.

``` r
DAGassist(
    dag = dag_model,
    formula = fixest::feols(Y ~ X + Z + i(region), data = df),
    omit_intercept = TRUE, omit_factors = TRUE # both TRUE by default
  )
```

### `bivariate`: include a no-covariate comparison column

Include a `Y ~ X` column for readers who want the raw association.
`bivariate = FALSE` by default.

``` r
DAGassist(
  dag = dag_model, 
  formula = lm(Y ~ X + C, data = df),
  show = "models",
  bivariate = TRUE
)
#> DAGassist Report: 
#> 
#> Model comparison:
#> 
#> +----------+----------+-----------+-----------+-----------+
#> |          | Original | Bivariate | Minimal 1 | Canonical |
#> +==========+==========+===========+===========+===========+
#> | X        | 0.908*** | 1.415***  | 1.415***  | 1.415***  |
#> +----------+----------+-----------+-----------+-----------+
#> |          | (0.030)  | (0.021)   | (0.021)   | (0.021)   |
#> +----------+----------+-----------+-----------+-----------+
#> | C        | 0.475*** |           |           |           |
#> +----------+----------+-----------+-----------+-----------+
#> |          | (0.022)  |           |           |           |
#> +----------+----------+-----------+-----------+-----------+
#> | Num.Obs. | 2000     | 2000      | 2000      | 2000      |
#> +----------+----------+-----------+-----------+-----------+
#> | R2       | 0.752    | 0.693     | 0.693     | 0.693     |
#> +==========+==========+===========+===========+===========+
#> | + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001       |
#> +==========+==========+===========+===========+===========+
```

### `verbose`: printing formulas & notes

`verbose` = TRUE (default) prints helpful notes (what was added/dropped,
derived formulas). Set to FALSE for a quieter console.

``` r
DAGassist(dag = dag_model, formula = Y ~ X + Z + C, data = df, verbose = FALSE)
```

## Parameter Reference Table

|    Parameter     |          Type          |                    Default                     | What it does                                                                      |
|:----------------:|:----------------------:|:----------------------------------------------:|:----------------------------------------------------------------------------------|
|      `dag`       |     dagitty object     |                       —                        | The DAG to validate and evaluate.                                                 |
|    `formula`     | formula or single call |                       —                        | Either `Y ~ X + ...` or a single engine call like `feols(...)`.                   |
|      `data`      |       data.frame       |                       —                        | Required unless supplied in engine call.                                          |
|     `engine`     |        function        | [`stats::lm`](https://rdrr.io/r/stats/lm.html) | Modeling function (ignored if `formula` is a call).                               |
|  `engine_args`   |       named list       |  [`list()`](https://rdrr.io/r/base/list.html)  | Extra args for `engine(...)`; merged with call args (call wins).                  |
|    `verbose`     |        logical         |                     `TRUE`                     | Print formulas & notes in console.                                                |
|      `type`      |         string         |                  `"console"`                   | One of `"console"`, `"latex"`, `"docx"/"word"`, `"xlsx"/"excel"`, `"text"/"txt"`. |
|      `out`       |          path          |                       —                        | Output path for non-console types.                                                |
|     `imply`      |        logical         |                    `FALSE`                     | Scope: pruned-to-formula vs full-DAG evaluation.                                  |
|     `labels`     | named chr / data.frame |                     `NULL`                     | Rename coefficients (modelsummary `coef_rename` logic).                           |
| `omit_intercept` |        logical         |                     `TRUE`                     | Hide intercept in printed comparison.                                             |
|  `omit_factors`  |        logical         |                     `TRUE`                     | Hide factor levels in printed comparison.                                         |
|      `show`      |         string         |                    `"all"`                     | `"all"`, `"roles"`, or `"models"`.                                                |
|    `eval_all`    |        logical         |                    `FALSE`                     | Keep non-DAG RHS terms (FEs, splines, interactions) in derived models.            |
