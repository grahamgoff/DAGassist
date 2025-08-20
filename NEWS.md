# DAGassist NEWS

## UNRELEASED

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
 
Next steps: 
- [x] `Validation: VALID` is super vague and I don't think there's acyclicity-check
    functionality in `validate.R`. Fix that output and add acyclicity function.
- [x] Adapt `print.DAGassist_report` to work with different models like feols, 
    specifically in the model compare. feols is snagging on "original fit 
    (coef head)". might be time to break out `stargazer` or `modelsummary`.
- [ ] Integrate canonical models
- [ ] Run `dag_assist` on a bunch of complex DAGS to refine the sorting 
    algorithm in `compare.R`.
