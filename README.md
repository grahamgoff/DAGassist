
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DAGassist

<!-- badges: start -->

[![R-CMD-check](https://github.com/grahamgoff/DAGassist/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/grahamgoff/DAGassist/actions/workflows/R-CMD-check.yaml)
[![pages-build-deployment](https://github.com/grahamgoff/DAGassist/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/grahamgoff/DAGassist/actions/workflows/pages/pages-build-deployment)
<!-- badges: end -->

**An all-in-one DAG-driven robustness check.** Classify variables by
causal role, compute the smallest and largest permissible back-door
adjustment sets, and compare the significance of models.

See the [Making
Reports](https://grahamgoff.github.io/DAGassist/articles/making-reports.html)
vignette for details on producing publication-quality DAGassist reports
in `LaTex`, `Word`, `Excel`, and `plaintext`.

See the [Getting
Started](https://grahamgoff.github.io/DAGassist/articles/getting-started.html)
vignette for examples of how to use and get the most out of DAGassist.

## Installation

You can install the development version of DAGassist from
[GitHub](https://github.com/grahamgoff/DAGassist) with:

``` r
install.packages("pak")
pak::pak("grahamgoff/DAGassist")
```

## DAGassist example

Simply provide a `dagitty()` object and a regression call and DAGassist
will create a report classifying variables by causal role, and compare
the specified regression to minimal and canonical models.

``` r
library(DAGassist) 

DAGassist(dag = dag_model, 
          formula = feols(Y ~ X + M + C + Z + A + B, data = df))
#> DAGassist Report: 
#> 
#> Roles:
#> variable  role        X  Y  conf  med  col  desc(Y)  desc(X)
#> X         exposure    x                                     
#> Y         outcome        x                           x      
#> Z         confounder        x                               
#> M         mediator                x                  x      
#> C         collider                     x    x        x      
#> A         other                                             
#> B         other                                             
#> 
#>  (!) Bad controls in your formula: {M, C}
#> Minimal controls 1: {Z}
#> Canonical controls: {A, B, Z}
#> 
#> Formulas:
#>   original:  Y ~ X + M + C + Z + A + B
#> 
#> Model comparison:
#> 
#> +-------------+-----------+-----------+-----------+
#> |             | Original  | Minimal 1 | Canonical |
#> +=============+===========+===========+===========+
#> | (Intercept) | -0.016    | -0.017    | -0.014    |
#> +-------------+-----------+-----------+-----------+
#> |             | (0.021)   | (0.027)   | (0.026)   |
#> +-------------+-----------+-----------+-----------+
#> | X           | 0.452***  | 1.256***  | 1.256***  |
#> +-------------+-----------+-----------+-----------+
#> |             | (0.032)   | (0.027)   | (0.026)   |
#> +-------------+-----------+-----------+-----------+
#> | M           | 0.514***  |           |           |
#> +-------------+-----------+-----------+-----------+
#> |             | (0.021)   |           |           |
#> +-------------+-----------+-----------+-----------+
#> | C           | 0.343***  |           |           |
#> +-------------+-----------+-----------+-----------+
#> |             | (0.019)   |           |           |
#> +-------------+-----------+-----------+-----------+
#> | Z           | 0.249***  | 0.311***  | 0.309***  |
#> +-------------+-----------+-----------+-----------+
#> |             | (0.027)   | (0.034)   | (0.033)   |
#> +-------------+-----------+-----------+-----------+
#> | A           | 0.152***  |           | 0.187***  |
#> +-------------+-----------+-----------+-----------+
#> |             | (0.021)   |           | (0.026)   |
#> +-------------+-----------+-----------+-----------+
#> | B           | -0.069*** |           | -0.057*   |
#> +-------------+-----------+-----------+-----------+
#> |             | (0.021)   |           | (0.026)   |
#> +-------------+-----------+-----------+-----------+
#> | Num.Obs.    | 2000      | 2000      | 2000      |
#> +-------------+-----------+-----------+-----------+
#> | R2          | 0.818     | 0.706     | 0.714     |
#> +=============+===========+===========+===========+
#> | + p < 0.1, * p < 0.05, ** p < 0.01, *** p <     |
#> | 0.001                                           |
#> +=============+===========+===========+===========+
```
