
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
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(DAGassist) 

# Load the shipped demo DAG and data
data(test_complex, package = "DAGassist")
data(test_df,      package = "DAGassist")

dag_assist(dag = test_complex, 
           formula = Y ~ X + M + C + Z,
           engine = lm,
           data = test_df, 
           exposure = "X",
           outcome = "Y")
#> $validation
#> DAGassist validation: INVALID
#> Issues: 7 error(s), 0 warning(s)
#> - [error] C - Variable in formula not found in DAG. (missing_in_dag) 
#> - [error] M - Variable in formula not found in DAG. (missing_in_dag) 
#> - [error] X - Exposure not found in DAG. (missing_in_dag) 
#> - [error] X - Variable in formula not found in DAG. (missing_in_dag) 
#> - [error] Y - Outcome not found in DAG. (missing_in_dag) 
#> - [error] Y - Variable in formula not found in DAG. (missing_in_dag) 
#> - [error] Z - Variable in formula not found in DAG. (missing_in_dag)
```
