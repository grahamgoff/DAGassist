
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DAGassist

<!-- badges: start -->
<!-- badges: end -->

**An all-in-one DAG-driven robustness check.** Classifies variables by
causal role, computes the smallest back-door adjustment set, and
compares the significance of the original and minimal models.

## Installation

You can install the development version of DAGassist from
[GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("grahamgoff/DAGassist")

#load the library
library(DAGassist) 
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
dag_assist(dag = test_complex, 
           formula = Y ~ X + M + C + Z,
           engine = lm,
           data = test_df, 
           exposure = "X",
           outcome = "Y")
#> DAGassist report
#> Validation: VALID
#> 
#> Roles:
#> variable  role        X  Y  conf  med  col  desc(Y)  desc(X)
#> X         exposure    x                                     
#> Y         outcome        x                           x      
#> Z         confounder        x                               
#> M         mediator                x                  x      
#> C         collider                     x    x        x      
#> A         other                                             
#> 
#>  (!) Bad controls in your formula: {M, C}
#> 
#> Minimal controls: {Z}
#> 
#> Formulas:
#>   original: Y ~ X + M + C + Z
#>   minimal : Y ~ X + Z
#> 
#> Original fit (coef head):
#>                Estimate Std. Error    t value     Pr(>|t|)
#> (Intercept)  0.01259982 0.02857765  0.4408975 6.595284e-01
#> X           -0.14353786 0.07616901 -1.8844654 6.023608e-02
#> M            0.34267779 0.03997889  8.5714675 2.334644e-16
#> C            0.69743534 0.03235632 21.5548403 1.094768e-68
#> Z            0.18215074 0.05378954  3.3863597 7.793151e-04
#> 
#> Minimal  fit (coef head):
#>               Estimate Std. Error    t value     Pr(>|t|)
#> (Intercept) 0.03307336 0.05073773  0.6518494 5.148755e-01
#> X           1.50834433 0.08531194 17.6803421 5.167703e-52
#> Z           0.49787901 0.09236472  5.3903590 1.208907e-07
```
