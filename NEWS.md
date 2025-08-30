# DAGassist NEWS

## Roadmap to v0.2.0:

- [x] Add acyclicity checks to validate.R
- [x] Clarify the validation output
- [x] Add neat tables to the console output
- [x] Include Canonical adjustment sets
- [x] Handling multiple minimal adjustment sets
- [x] Add more colors in output
- [x] Add options for LaTeX output
- [x] Add word output functionality
- [x] Add excel output functionality
- [x] Add plaintext output functionality
- [x] Add export_report functionality, which will print a publication-grade 2-page robustness check with a single, simple call (for robustness check use)
- [x] Add `imply`, default TRUE, logical parameter to disable adding variables to formula/table based on DAG relationships.
- [ ] Ensure that classify.R categories are **always** accurate
- [ ] Verify Engine compatibility with all major engines
- [ ] Handle diff-in-diff and fixed effects formula notation
- [ ] Add graphics: gray scale of the original model, with the minimal set visibly differentiated.
- [ ] Add a simple "covariate sensitivity" line to report, indicating if (i) the original model is statistically significant; (ii) the minimal or canonical set is statistically insignificant
- [ ] Run `dag_assist` on a bunch of complex DAGS to refine the sorting algorithm in `compare.R`.
- [ ] Change `dag_assist` to `DAGassist`.
- [ ] Get rid of the engine args param; it is obsolete

## UNRELEASED

### 0.2.0.9100 - 2025-08-28

#### Added
- report_latex.R provides LaTeX output functionality

#### Changed
- **Added some pretty colors to console report output**

### 0.2.0.9000 - 2025-08-20

#### Changed

- **Clarified validation output and added acyclicity check to `assist.R`**
- **Adapted the report printer in `assist.R` to be model-agnostic**
  - Now accepts most common models
- **Added table to report**
  - `modelsummary()` preferred, `broom` secondary, coef head fall-back
- **Added canonical adjustment sets to the table and model comparison**
- **Added multiple minimal adjustment set functionality**
- **Added `verbose` parameter to `dag_assist` to suppress formula and notes**
  - Given the report states the adjustment sets, formulas + notes about imputed controls could be a lot.

### 0.1.0 — 2025-08-19

#### Added 
- NEWS.md initialized

#### Changed

-**Simplified `dag_assist` arguments.**
  - Now accepts a single engine call and **infers** `formula`, `data`, `engine`, and `engine_args` from it.
  - **Infers** `exposure` and `outcome` from the `dagitty` object when omitted (only if each is uniquely specified in the DAG).
    
  **BEFORE(verbose and fragile):**
```r
  dag_assist(dag = test_complex, 
           formula = Y ~ X + M + C + Z,
           engine = feols, # didn't work previously (engine-specific handling)
           engine_args = list() # not very functional
           data = test_df,
           exposure = "X", 
           outcome = "Y")
```
  

  **NOW: (plug-and-play):**
```r
  dag_assist(
    dag = test_complex, 
    formula = feols(Y ~ X + M + C + Z | region, 
    data = test_df)
  )
```

- **Validation respects fixest-style formulas.**
  - `validate.R` now validates only the base part of the formula (pre-`|`), so fixed effects / IV parts (e.g., id, time, region) are not required to appear in the DAG.
