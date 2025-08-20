
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DAGassist

<!-- badges: start -->

[![R-CMD-check](https://github.com/grahamgoff/DAGassist/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/grahamgoff/DAGassist/actions/workflows/R-CMD-check.yaml)
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
dag_assist(dag=test_complex, formula = feols(Y ~ X + Z + B + A + C + M | region + time, data = test_df))
DAGassist Report:

Roles:
variable  role        X  Y  conf  med  col  desc(Y)  desc(X)
X         exposure    x                                     
Y         outcome        x                           x      
Z         confounder        x                               
M         mediator                x                  x      
C         collider                     x    x        x      
A         other                                             
B         other                                             

 (!) Bad controls in your formula: {C, M}
Minimal controls 1: {Z}
Canonical controls: {A, B, Z}

Formulas:
  original:  Y ~ X + Z + B + A + C + M | region + time
  minimal 1 : Y ~ X + Z | region + time
  canonical: Y ~ X + A + B + Z | region + time

Model comparison:

+------------+----------+-----------+-----------+
|            | Original | Minimal 1 | Canonical |
+============+==========+===========+===========+
| X          | -0.054   | 1.371***  | 1.348***  |
+------------+----------+-----------+-----------+
|            | (0.122)  | (0.076)   | (0.055)   |
+------------+----------+-----------+-----------+
| Z          | 0.283*** | 0.637***  | 0.660***  |
+------------+----------+-----------+-----------+
|            | (0.048)  | (0.101)   | (0.072)   |
+------------+----------+-----------+-----------+
| B          | 0.024    |           | 0.023     |
+------------+----------+-----------+-----------+
|            | (0.029)  |           | (0.040)   |
+------------+----------+-----------+-----------+
| A          | 0.024    |           | 0.265***  |
+------------+----------+-----------+-----------+
|            | (0.040)  |           | (0.032)   |
+------------+----------+-----------+-----------+
| C          | 0.669*** |           |           |
+------------+----------+-----------+-----------+
|            | (0.046)  |           |           |
+------------+----------+-----------+-----------+
| M          | 0.216*   |           |           |
+------------+----------+-----------+-----------+
|            | (0.075)  |           |           |
+------------+----------+-----------+-----------+
| Num.Obs.   | 400      | 400       | 400       |
+------------+----------+-----------+-----------+
| R2         | 0.933    | 0.814     | 0.828     |
+------------+----------+-----------+-----------+
| FE: region | X        | X         | X         |
+------------+----------+-----------+-----------+
| FE: time   | X        | X         | X         |
+============+==========+===========+===========+
| + p < 0.1, * p < 0.05, ** p < 0.01, *** p <   |
| 0.001                                         |
+============+==========+===========+===========+ 
```

## How to get the most out of **DAGassist**

### See each variable’s causal role:

``` r
classify_nodes(test_complex, exposure = "X", outcome = "Y")
variable  role        X  Y  conf  med  col  desc(Y)  desc(X)
X         exposure    x                                     
Y         outcome        x                           x      
Z         confounder        x                               
M         mediator                x                  x      
C         collider                     x    x        x      
A         other                                             
B         other                                             
```

### Quickly flag bad controls:

``` r
bad_controls_in(dag = test_complex, 
                controls = c("Z", "M", "C"), 
                exposure = "X",
                outcome = "Y")
[1] "M" "C"
```
