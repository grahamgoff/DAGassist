# DAGassist 0.1.0

### 8/19/25

- Added NEWS.md 

- Simplified dag_assist arguments:
    -Modified `assist.R` to imply formula, data, engine, and engine_args from the
    formula call 
    -AND imply the exposure and outcome arguments from the dagitty object, which
    is a safe bet because `validate.R` already makes sure the dagitty object contains
    explicit (and single) exposure and outcome specs.
    
  *BEFORE:* (confusing)
  dag_assist(dag = test_complex, 
           formula = Y ~ X + M + C + Z,
           engine = feols, (didn't work before because `assist.R` was not engine agnostic)
           engine_args = (this didn't really ever work)
           data = test_df, 
           exposure = "X",
           outcome = "Y")
  *NOW:* (plug-and-play)
  dag_assist(dag = test_complex, formula = feols(Y ~ X + M + C + Z | region, 
             data = test_df))
    
-Modified `validate.R`, which was throwing errors because it previously checking the
 full formula call to make sure *everything* matched with the DAG. Previously, this
 was fine because the formula argument was only supposed to take a non-engine 
 specific formula. Since we can include fixed effects variables that might
 not be in the DAG, I made `validate.R` only look at stuff before the `|`, which
 fixed the problem. I imagine I will have to make similar changes as I try out
 new engines. 
 
Next steps:
[ ] `Validation: VALID` is super vague and I don't think there's acyclicity-check
    functionality in `validate.R`. Fix that output and add acyclicity function.
[ ] Adapt `print.DAGassist_report` to work with different models like feols, 
    specifically in the model compare. feols is snagging on "original fit 
    (coef head)". might be time to break out `stargazer` or `modelsummary`.
[ ] Integrate canonical models
[ ] Run `dag_assist` on a bunch of complex DAGS to refine the sorting 
    algorythm in `compare.R`.
