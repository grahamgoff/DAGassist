
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
dag_assist(dag = test_complex, 
           formula = Y ~ X + M + C + Z,
           engine = lm,
           data = test_df, 
           exposure = "X",
           outcome = "Y")
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

 (!) Bad controls in your formula: {M, C}

Minimal controls: {Z}
Canonical controls: {Z}

Formulas:
  original:  Y ~ X + M + C + Z
  minimal :  Y ~ X + Z
  canonical: Y ~ X + Z

Note: some specifications are identical (Minimal = Canonical).
Estimates will match for those columns.

Model comparison:

+-------------+----------+----------+-----------+
|             | Original | Minimal  | Canonical |
+=============+==========+==========+===========+
| (Intercept) | 0.011    | 0.015    | 0.015     |
+-------------+----------+----------+-----------+
|             | (0.030)  | (0.045)  | (0.045)   |
+-------------+----------+----------+-----------+
| X           | -0.082   | 1.388*** | 1.388***  |
+-------------+----------+----------+-----------+
|             | (0.082)  | (0.074)  | (0.074)   |
+-------------+----------+----------+-----------+
| M           | 0.255*** |          |           |
+-------------+----------+----------+-----------+
|             | (0.042)  |          |           |
+-------------+----------+----------+-----------+
| C           | 0.648*** |          |           |
+-------------+----------+----------+-----------+
|             | (0.035)  |          |           |
+-------------+----------+----------+-----------+
| Z           | 0.343*** | 0.645*** | 0.645***  |
+-------------+----------+----------+-----------+
|             | (0.054)  | (0.078)  | (0.078)   |
+-------------+----------+----------+-----------+
| Num.Obs.    | 400      | 400      | 400       |
+-------------+----------+----------+-----------+
| R2          | 0.931    | 0.841    | 0.841     |
+=============+==========+==========+===========+
| + p < 0.1, * p < 0.05, ** p < 0.01, *** p <   |
| 0.001                                         |
+=============+==========+==========+===========+ 
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

## Roadmap to v2.0:

- [x] Add acyclicity checks to validate.R
- [x] Clarify the validation output
- [x] Add neat tables to the console output
- [x] Include Canonical adjustment sets
- [ ] Ensure that classify.R categories are **always** accurate
- [ ] Handling multiple minimal adjustment sets
- [ ] Engine compatibility with all major engines
- [ ] Handle diff-in-diff and fixed effects formula notation
- [ ] Add graphics: gray scale of the original model, with the minimal
  set visibly differentiated.
- [ ] Add more colors in output
- [ ] Add options for LaTeX and Kable output
- [ ] Add export_report functionality, which will print a
  publication-grade 2-page robustness check with a single, simple call
  (for robustness check use)
- [ ] Add a simple “covariate sensitivity” line to report, indicating
  if (i) the original model is statistically significant; (ii) the
  minimal or canonical set is statistically insignificant
