
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
FALSE DAGassist report
FALSE Validation: VALID
FALSE 
FALSE Roles:
FALSE variable  role        X  Y  conf  med  col  desc(Y)  desc(X)
FALSE X         exposure    x                                     
FALSE Y         outcome        x                           x      
FALSE Z         confounder        x                               
FALSE M         mediator                x                  x      
FALSE C         collider                     x    x        x      
FALSE A         other                                             
FALSE 
FALSE  (!) Bad controls in your formula: {M, C}
FALSE 
FALSE Minimal controls: {Z}
FALSE 
FALSE Formulas:
FALSE   original: Y ~ X + M + C + Z
FALSE   minimal : Y ~ X + Z
FALSE 
FALSE Original fit (coef head):
FALSE                Estimate Std. Error    t value     Pr(>|t|)
FALSE (Intercept)  0.01259982 0.02857765  0.4408975 6.595284e-01
FALSE X           -0.14353786 0.07616901 -1.8844654 6.023608e-02
FALSE M            0.34267779 0.03997889  8.5714675 2.334644e-16
FALSE C            0.69743534 0.03235632 21.5548403 1.094768e-68
FALSE Z            0.18215074 0.05378954  3.3863597 7.793151e-04
FALSE 
FALSE Minimal  fit (coef head):
FALSE               Estimate Std. Error    t value     Pr(>|t|)
FALSE (Intercept) 0.03307336 0.05073773  0.6518494 5.148755e-01
FALSE X           1.50834433 0.08531194 17.6803421 5.167703e-52
FALSE Z           0.49787901 0.09236472  5.3903590 1.208907e-07
```

## Roadmap to v2.0:

- [ ] Ensure that classify.R categories are **always** accurate
- [ ] Add acyclicity checks to validate.R
- [ ] Include Canonical adjustment sets
- [ ] Handling multiple minimal adjustment sets
- [ ] Engine compatibility with all major engines
- [ ] Handle diff-in-diff and fixed effects formula notation
- [ ] Add graphics and colors - \[ \] gray scale of the original model,
  with the minimal set visibly differentiated. - \[ \] more colors in
  output
- [ ] Clarify the validation output
- [ ] Add neat tables to the console output
- [ ] Add options for LaTeX and Kable output
- [ ] Add export_report functionality, which will print a
  publication-grade 2-page robustness check with a single, simple call
  (for robustness check use)
- [ ] Add a simple “covariate sensitivity” line to report, indicating
  if (i) the original model is statistically significant; (ii) the
  minimal or canonical set is statistically insignificant
