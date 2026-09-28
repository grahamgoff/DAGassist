# Why DAGassist?

Choosing control variables is one of the most consequential decisions in
observational research, and one of the least scrutinized. The common
practice of controlling for any plausible common cause of the treatment
and outcome may introduce bias ([Achen 2005](#ref-Achen2005); [Cinelli
et al. 2022](#ref-CinelliForneyPearl2022)). `DAGassist` provides a way
to systematize adjustment decisions and report the results.

## Regression output can’t tell you which controls are wrong

The consequences of controlling for a variable depend on its causal
role:

- Controlling for a **confounder** removes bias.
- Controlling for a **mediator** changes the estimand, from a total
  effect to something closer to a direct effect.
- Controlling for a **collider** or a **descendant of the outcome**
  introduces bias ([Montgomery et al.
  2018](#ref-MontgomeryNyhanTorres2018)).

These distinctions are not visible in regression output. Consider two
regressions of voter turnout on income using the simulated
`turnout_data` included with the package. The first controls for every
available variable; the second controls only for the confounders, age
and state.

``` r

library(DAGassist)
library(modelsummary)

all_controls <- lm(turnout ~ income + state + age + polint + industry + elect_comp,
                   data = turnout_data)
confounders  <- lm(turnout ~ income + state + age, data = turnout_data)

models <- list(all_controls, confounders)

modelsummary(
  models,
  coef_map = c("income" = "Income"),
  stars = TRUE,
  gof_omit = ".*"
)
```

|  | \(1\) | \(2\) |
|----|----|----|
| Income | 0.281\*\*\* | 0.493\*\*\* |
|  | (0.016) | (0.016) |
| \+ p \< 0.1, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001 |  |  |

Both estimates are precise and highly significant, but they do not
estimate the same quantity. Distinguishing between them requires
assumptions about how the variables are causally related.

## The gap between DAGs and regressions

Directed acyclic graphs (DAGs) provide a systematic framework for
choosing control variables ([Pearl 2009](#ref-Pearl2009); [Elwert
2013](#ref-Elwert2013); [Hünermund et al.
2025](#ref-HunermundEtAl2025)). Several tools already make it easy to
work with DAGs in R. `dagitty` provides a syntax for specifying DAGs and
deriving adjustment sets ([Textor et al. 2016](#ref-TextorEtAl2016)),
while `ggdag` provides tools for plotting them.

What is less straightforward is checking whether an estimated regression
actually follows from the DAG used to justify it. Reviews of applied
research find that researchers’ adjustment decisions do not always
correspond to the DAGs they report ([Tennant et al.
2021](#ref-TennantEtAl2021)). That discrepancy can also be difficult for
readers or reviewers to detect from the information typically reported.

`DAGassist` is designed to close the gap between representation and
estimation. Given a DAG and a fitted regression, it identifies the
causal role of each control, flags controls that alter the estimand or
induce bias according to the DAG, and re-fits the model using
DAG-implied adjustment sets. It can target a specified estimand and
examine whether the conclusions depend on uncertain assumptions within
the DAG. For the turnout example, a single call identifies the roles of
the included controls:

``` r

DAGassist(turnout_dag,
          lm(turnout ~ income + state + age + polint + industry + elect_comp,
             data = turnout_data),
          show = "roles", verbose = FALSE)
#> DAGassist Report: 
#> 
#> Roles:
#> variable    role        Exp.  Out.  conf  med  col  dOut  dMed  dCol  dConfOn  dConfOff  NCT  NCO
#> income      exposure    x                                                                        
#> turnout     outcome           x                                                                  
#> age         confounder              x                                                            
#> state       confounder              x                                                            
#> polint      mediator                      x                                                      
#> elect_comp  nco                                                                               x  
#> industry    nct                                                       x                  x       
#> 
#>  (!) Bad controls in your formula: {polint}
#> 
#> Legend hidden because verbose = FALSE. Re-run with verbose = TRUE to see role definitions.
```

## Design principles

**Start from the researcher’s model.** Pass a regression formula through
[`lm()`](https://rdrr.io/r/stats/lm.html),
[`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html),
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html), or another
estimator with a formula interface ([supported
engines](https://grahamgoff.com/DAGassist/articles/compatibility.md)).
`DAGassist` re-fits the model using the same estimator and options while
only changing the adjustment set.

**Keep the original specification visible.** The original model appears
alongside the DAG-derived specifications. The purpose is to show how
estimates change under different adjustment decisions, not to replace
the researcher’s preferred model.

**Make the estimand explicit.** Because adjustment decisions can change
the quantity being estimated, `DAGassist` distinguishes between the
average total effect and average controlled direct effect ([Lundberg et
al. 2021](#ref-LundbergJohnsonStewart2021); [Acharya et al.
2016](#ref-AcharyaBlackwellSen2016)).

**Allow uncertainty about the DAG.** Everything follows from the DAG, so
`DAGassist` checks how the adjustment sets change if arrows are reversed
or missing ([Haber et al. 2022](#ref-HaberEtAl2022)).

**Make the results easy to report.** Results can be exported as LaTeX,
Word, Excel, plain text, and dotwhisker plots, making it possible to
include the analysis in an appendix or response to reviewers.

**Use existing tools where possible.** `DAGassist` relies on dagitty for
graph analysis, `WeightIt` and `marginaleffects` for weighting,
`DirectEffects` for sequential g-estimation, and `modelsummary` for
tables. Its role is to connect these tools to the regression
specification being evaluated.

## Where DAGassist fits

| Task | Established tools | What `DAGassist` adds |
|:---|:---|:---|
| Draw and analyze a DAG | `dagitty`, `ggdag`, the DAGitty web tool | Accepts `dagitty` and `ggdag` DAGs as input |
| Find adjustment sets | [`dagitty::adjustmentSets()`](https://rdrr.io/pkg/dagitty/man/adjustmentSets.html) | Compares the model’s controls with DAG-implied adjustment sets |
| Fit models | [`lm()`](https://rdrr.io/r/stats/lm.html), `fixest`, `lme4`, and others | Re-fits the original specification using DAG-derived adjustment sets |
| Target an estimand | `WeightIt`, `marginaleffects`, `DirectEffects` | Builds the weighting and sequential g-estimation specifications from the DAG |
| Question the DAG | [`dagitty::localTests()`](https://rdrr.io/pkg/dagitty/man/localTests.html) | Examines how adjustment sets and variable roles change under uncertain or missing edges |
| Unmeasured confounding | `sensemakr` ([Cinelli and Hazlett 2020](#ref-CinelliHazlett2020)) | Identifies when a hypothesized unmeasured confounder prevents identification by adjustment |
| Report | `modelsummary` | Exports the whole diagnostic, including variable roles and model comparisons, in one call |

## What DAGassist does not do

- **It doesn’t tell you whether your DAG is right.** DAGassist does not
  learn causal structure from the data. Its conclusions are conditional
  on the DAG supplied by the researcher. The robustness functions
  [robustness
  functions](https://grahamgoff.com/DAGassist/articles/robustness.md)
  can be used to examine whether conclusions depend on uncertain or
  missing edges.
- **It identifies effects by adjustment only.** Designs that rely on
  other sources of identification, such as instrumental variables, the
  front-door criterion, difference-in-differences, or regression
  discontinuity, are outside its scope.

## Who it’s for

DAGassist is intended for researchers using regression with
observational data who want to check and document their adjustment
decisions. It can also help reviewers and readers evaluate how a paper’s
causal assumptions inform its empirical specification. For instructors,
the package includes known-answer datasets (`turnout_data` and
`toy_data`) that illustrate the consequences of good and bad controls
concretely.

To try it, see [Get
started](https://grahamgoff.com/DAGassist/articles/DAGassist.md).

## References

Acharya, Avidit, Matthew Blackwell, and Maya Sen. 2016. “Explaining
Causal Findings Without Bias: Detecting and Assessing Direct Effects.”
*American Political Science Review* 110 (3): 512–29.
<https://doi.org/10.1017/S0003055416000216>.

Achen, Christopher H. 2005. “Let’s Put Garbage-Can Regressions and
Garbage-Can Probits Where They Belong.” *Conflict Management and Peace
Science* 22 (4): 327–39. <https://doi.org/10.1080/07388940500339167>.

Cinelli, Carlos, Andrew Forney, and Judea Pearl. 2022. “A Crash Course
in Good and Bad Controls.” *Sociological Methods & Research*, ahead of
print. <https://doi.org/10.1177/00491241221099552>.

Cinelli, Carlos, and Chad Hazlett. 2020. “Making Sense of Sensitivity:
Extending Omitted Variable Bias.” *Journal of the Royal Statistical
Society: Series B (Statistical Methodology)* 82 (1): 39–67.
<https://doi.org/10.1111/rssb.12348>.

Elwert, Felix. 2013. “Graphical Causal Models.” In *Handbook of Causal
Analysis for Social Research*, edited by Stephen L. Morgan, vol. 54.
Springer. <https://doi.org/10.1007/978-1-4471-6699-3_13>.

Haber, Noah A., Mollie E. Wood, Sarah Wieten, and Alexander Breskin.
2022. “DAG with Omitted Objects Displayed (DAGWOOD): A Framework for
Revealing Causal Assumptions in DAGs.” *Annals of Epidemiology* 68
(April): 64–71. <https://doi.org/10.1016/j.annepidem.2022.01.001>.

Hünermund, Paul, Beyers Louw, and Mikko Rönkkö. 2025. “The Choice of
Control Variables in Empirical Management Research: How Causal Diagrams
Can Inform the Decision.” *Leadership Quarterly* 36: 1–15.

Lundberg, Ian, Rebecca Johnson, and Brandon M. Stewart. 2021. “What Is
Your Estimand? Defining the Target Quantity Connects Statistical
Evidence to Theory.” *American Sociological Review* 86: 532–65.
<https://doi.org/10.1177/00031224211004187>.

Montgomery, Jacob M., Brendan Nyhan, and Michelle Torres. 2018. “How
Conditioning on Posttreatment Variables Can Ruin Your Experiment and
What to Do about It.” *American Journal of Political Science* 62 (3):
760–75. <https://doi.org/10.1111/ajps.12357>.

Pearl, Judea. 2009. *Causality: Models, Reasoning, and Inference*.
Cambridge University Press.

Tennant, Peter W. G., Eleanor J. Murray, Kellyn F. Arnold, et al. 2021.
“Using Directed Acyclic Graphs (DAGs) to Identify Confounders in Applied
Research: Review and Recommendatsion.” *International Journal of
Epidemiology* 50 (2): 620–32. <https://doi.org/10.1093/ije/dyaa213>.

Textor, Johannes, Benito van der Zander, Mark S. Gilthorpe, Maciej
Liśkiewicz, and George T. H. Ellison. 2016. “Robust Causal Inference
Using Directed Acyclic Graphs: The R Package ‘Dagitty’.” *International
Journal of Epidemiology* 45 (6): 1887–94.
<https://doi.org/10.1093/ije/dyw341>.
