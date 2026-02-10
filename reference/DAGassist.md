# Generate and/or export report classifying nodes and comparing models

`DAGassist()` validates a DAG + model specification, classifies node
roles, builds minimal and canonical adjustment sets, fits comparable
models, and renders a compact report in several formats (console, LaTeX
fragment, DOCX, XLSX, plain text). It also supports recovering new
estimands, such as the sample average treatment effect (SATE) or the
sample average controlled direct effect (SACDE).

## Usage

``` r
DAGassist(
  dag,
  formula = NULL,
  data = NULL,
  exposure,
  outcome,
  engine = stats::lm,
  labels = NULL,
  verbose = TRUE,
  type = c("console", "latex", "word", "docx", "excel", "xlsx", "text", "txt", "dwplot",
    "dotwhisker"),
  show = c("all", "roles", "models"),
  out = NULL,
  imply = FALSE,
  eval_all = FALSE,
  exclude = NULL,
  omit_intercept = TRUE,
  omit_factors = TRUE,
  bivariate = FALSE,
  estimand = c("raw", "none", "SATE", "SATT", "SACDE", "SCDE"),
  engine_args = list(),
  weights_args = list(),
  auto_acde = TRUE,
  acde = list(),
  directeffects_args = list()
)
```

## Arguments

- dag:

  A **dagitty** object (see
  [`dagitty::dagitty()`](https://rdrr.io/pkg/dagitty/man/dagitty.html)).

- formula:

  Either (a) a standard model formula `Y ~ X + ...`, or (b) a single
  **engine call** such as `feols(Y ~ X + Z | fe, data = df, ...)`. When
  an engine call is provided, `engine`, `data`, and extra arguments are
  automatically extracted from the call.

- data:

  A `data.frame` (or compatible, e.g. tibble). Optional if supplied via
  the engine call in `formula`.

- exposure:

  Optional character scalar; if missing/empty, inferred from the DAG
  (must be unique).

- outcome:

  Optional character scalar; if missing/empty, inferred from the DAG
  (must be unique).

- engine:

  Modeling function, default
  [stats::lm](https://rdrr.io/r/stats/lm.html). Ignored if `formula` is
  a single engine call (in that case the function is taken from the
  call).

- labels:

  Optional variable labels (named character vector or data.frame).

- verbose:

  Logical (default `TRUE`). Controls verbosity in the console printer
  (formulas + notes).

- type:

  Output type. One of `"console"` (default),
  `"latex"`/`"docx"`/`"word"`, `"excel"`/`"xlsx"`, `"text"`/`"txt"`, or
  the plotting types `"dwplot"`/`"dotwhisker"`. For `type = "latex"`, if
  no `out=` is supplied, a LaTeX fragment is printed to the console
  instead of being written to disk.

- show:

  Which sections to include in the output. One of `"all"` (default),
  `"roles"` (only the roles grid), or `"models"` (only the model
  comparison table/plot). This makes it possible to generate and export
  just roles or just comparisons.

- out:

  Output file path for the non-console types:

  - `type="latex"`: a **LaTeX fragment** written to `out` (usually
    `.tex`); when omitted, the fragment is printed to the console.

  - `type="text"`/`"txt"`: a **plain-text** file written to `out`; when
    omitted, the report is printed to console.

  - `type="dotwhisker"`/`"dwplot"`: a **image (.png)** file written to
    `out`; when omitted, the plot is rendered within RStudio.

  - `type="docx"`/`"word"`: a **Word (.docx)** file written to `out`.

  - `type="excel"`/`"xlsx"`: an **Excel (.xlsx)** file written to `out`.
    Ignored for `type="console"`.

- imply:

  Logical; default `FALSE`. Specifies **evaluation scope.**

  - If `FALSE` (default): restrict DAG evaluation to variables **named
    in the formula** (prune the DAG to exposure, outcome, and RHS
    terms). Roles/sets/bad-controls are computed on this pruned graph,
    and the roles table **only** shows those variables. Essentially, it
    fits the DAG to the formula.

  - If `TRUE`: evaluate on the **full DAG** and allow DAG-implied
    controls in the minimal/canonical sets. The roles table shows all
    DAG nodes, and the printout notes any variables added beyond your
    RHS. Essentially, it fits the formula to the DAG.

- eval_all:

  Logical; default `FALSE`. When `TRUE`, keep **all original RHS terms**
  that are not in the DAG (e.g., fixed effects, interactions, splines,
  convenience covariates) in the minimal and canonical formulas. When
  `FALSE` (default), RHS terms not present as DAG nodes are dropped from
  those derived formulas.

- exclude:

  Optional character vector to remove neutral controls from the
  canonical set. Recognized values are `"nct"` (drop
  *neutral-on-treatment* controls) and `"nco"` (drop
  *neutral-on-outcome* controls). You can supply one or both, e.g.
  `exclude = c("nco", "nct")`; each requested variant is fitted and
  shown as a separate "Canon. (-...)" column in the console/model
  exports.

- omit_intercept:

  Logical; drop intercept rows from the model comparison display
  (default `TRUE`).

- omit_factors:

  Logical; drop factor-level rows from the model comparison display
  (default `TRUE`). This parameter only suppresses factor
  **output**–they are still included in the regression.

- bivariate:

  Logical; if `TRUE`, include a bivariate (exposure-only) specification
  in the comparison table **in addition** to the user's original and
  DAG-derived models.

- estimand:

  Character; causal estimand for the *reported columns* in the console
  output. One of `"raw"` (default), `"SATE"`, `"SATT"`, `"SACDE"` (alias
  `"SCDE"`), or `"none"`.

  - `"raw"`: reports the naive regression fits implied by the supplied
    engine/formulas.

  - `"SATE"` / `"SATT"`: adds inverse-probability weighted versions of
    each comparison model (via WeightIt) to target the *sample* ATE/ATT
    rather than the OLS implicit estimand.

  - `"SACDE"` / `"SCDE"`: for DAGs with mediator(s), adds **two**
    sequential g-estimation columns: (i) **Raw (SACDE)**: the unweighted
    DirectEffects sequential-g estimator, which—because the second stage
    is linear regression with controls—targets a *conditional-variance
    weighted* average of unit-level controlled direct effects (not a
    sample-average CDE). (ii) **Weighted (SACDE)**: re-runs sequential-g
    with IPW weights (estimated *without conditioning on mediators*) so
    the second-stage regression recovers the **sample average controlled
    direct effect (SACDE)** rather than the regression-weighted
    estimand.

- engine_args:

  Named list of extra arguments forwarded to `engine(...)`. If `formula`
  is an engine call, arguments from the call are merged with
  `engine_args` (call values take precedence).

- weights_args:

  List; arguments forwarded to WeightIt when computing IPW weights for
  `"SATE"`/`"SATT"` and for the **Weighted (SACDE)** refit. For SACDE,
  DAGassist estimates weights on the complete-case sample using the
  *baseline covariates* from the sequential-g block-1 specification
  (excluding mediator terms), by default via
  `WeightIt::weightit(..., method = "glm", estimand = "ATE")`. If
  `trim_at` is supplied, weights are winsorized at the requested
  quantile before refitting sequential-g.

- auto_acde:

  Logical; if `TRUE` (default), automates handling conflicts between
  specifications and estimand arguments. Fails gracefully with a helpful
  error when users specify ACDE estimand for a model without mediators.

- acde:

  List; options for the controlled direct effect workflow (estimands
  `"SACDE"`/`"SCDE"`). Users can override parts of the sequential
  g-estimation specification with named elements: `m` (mediators), `x`
  (baseline covariates), `z` (intermediate covariates), `fe`
  (fixed-effects variables), `fe_as_factor` (wrap `fe` as
  [`factor()`](https://rdrr.io/r/base/factor.html)), and
  `include_descendants` (treat descendants of mediators as mediators).

- directeffects_args:

  Named list of arguments forwarded to
  [`DirectEffects::sequential_g()`](https://mattblackwell.github.io/DirectEffects/reference/sequential_g.html)
  when `estimand` includes `"ACDE"`/`"CDE"` (e.g., simulation/bootstrap
  controls, variance estimator options).

## Value

A `DAGassist_report` object (a named list) returned invisibly for
file/plot outputs and printed for `type = "console"`.

The object contains:

- validation:

  List. Output of `validate_spec()`: DAG validity + exposure/outcome
  checks.

- roles:

  `data.frame`. Raw node-role flags from
  [`classify_nodes()`](https://grahamgoff.github.io/DAGassist/reference/classify_nodes.md).

- roles_display:

  `data.frame`. Roles table formatted for printing/export.

- labels_map:

  Named character vector. Variable → display label map used in
  tables/plots.

- controls_minimal:

  Character vector. (Legacy) One minimal adjustment set.

- controls_minimal_all:

  List of character vectors. All minimal adjustment sets.

- controls_canonical:

  Character vector. Canonical adjustment set (possibly empty).

- controls_canonical_excl:

  Named list. Filtered canonical sets created by `exclude`.

- conditions:

  List. Parsed conditional statements from the DAG (if any).

- formulas:

  List. User formula plus DAG-derived formula variants
  (minimal/canonical/etc.).

- models:

  List. Fitted models for each formula variant (including minimal-list
  fits).

- bad_in_user:

  Character vector. RHS terms classified as mediator/collider/etc.

- unevaluated:

  Character vector. Terms carried through but not evaluated by the
  engine.

- unevaluated_str:

  Character scalar. Pretty-printed version of `unevaluated`.

- settings:

  List. Print/export settings, including `coef_omit` and `show`.

- .\_\_data:

  `data.frame` or `NULL`. The data used to fit models (stored for
  downstream helpers).

For file outputs (`type = "latex"`, `"docx"`, `"xlsx"`, `"txt"`,
`"dotwhisker"`), the returned object includes attribute `file`, the
normalized output path.

## Details

**Engine-call parsing.** If `formula` is a call (e.g.,
`feols(Y ~ X | fe, data=df)`), DAGassist extracts the engine function,
formula, data argument, and any additional engine arguments directly
from that call; these are merged with `engine`/`engine_args` you pass
explicitly (call arguments win).

**fixest tails.** For engines like **fixest** that use `|` to denote
FE/IV parts, DAGassist preserves any `| ...` tail when constructing
minimal/canonical formulas (e.g., `Y ~ X + controls | fe | iv(...)`).

**Roles grid.** The roles table displays short headers:

- `Exp.` (exposure),

- `Out.` (outcome),

- `CON` (confounder),

- `MED` (mediator),

- `COL` (collider),

- `dOut` (descendant of `Y`),

- `dMed` (descendant of any mediator),

- `dCol` (descendant of any collider),

- `dConfOn` (descendant of a confounder **on** a back-door path),

- `dConfOff` (descendant of a confounder **off** a back-door path),

- `NCT` (neutral control on treatment),

- `NCO` (neutral control on outcome). These extra flags are used to (i)
  warn about bad controls, and (ii) build filtered canonical sets such
  as “Canonical (–NCO)” for export.

**Bad controls.** For total-effect estimation, DAGassist flags as
`bad controls` any variables that are `MED`, `COL`, `dOut`, `dMed`, or
`dCol`. These are warned in the console and omitted from the
model-comparison table. Valid confounders (pre-treatment) are eligible
for minimal/canonical adjustment sets.

**Output types.**

- `console` prints roles, adjustment sets, formulas (if `verbose`), and
  a compact model comparison (using `{modelsummary}` if available,
  falling back gracefully otherwise).

- `latex` writes or prints a **LaTeX fragment** you can `\\input{}` into
  a paper — it uses `tabularray` long tables and will include any
  requested Canon. (-NCO / -NCT) variants.

- `docx`/`word` writes a **Word** doc (respects
  `options(DAGassist.ref_docx=...)` if set).

- `excel`/`xlsx` writes an **Excel** workbook with tidy tables.

- `text`/`txt` writes a **plain-text** report for logs/notes.

- `dwplot`/`dotwhisker` produces a dot-whisker visualization of the
  fitted models.

**Dependencies.** Core requires `{dagitty}`. Optional enhancements:
`{modelsummary}` (pretty tables), `{broom}` (fallback tidying),
`{rmarkdown}` + **pandoc** (DOCX), `{writexl}` (XLSX),
`{dotwhisker}`/`{ggplot2}` for plotting.

**Raw vs Weighted SACDE.** The unweighted sequential-g estimator in
DirectEffects uses linear regression in its second stage. By the
Frisch–Waugh–Lovell theorem, this implies an estimand that is weighted
by the conditional variance of the (residualized) exposure given
controls—i.e., a regression-weighted average of unit-level effects, not
a sample-average controlled direct effect. DAGassist therefore reports
both the raw sequential-g result and a weighted sequential-g refit
(using WeightIt IPW weights estimated without mediators) to target the
*sample average* controlled direct effect.

## Interpreting the output

See the vignette articles for worked examples on generating roles-only,
models-only, and LaTeX/Word/Excel reports.

**Model Comparison:**

- **Minimal** - the smallest adjustment set that blocks all back-door
  paths (confounders only).

- **Canonical** - the largest permissible set: includes all controls
  that are not `MED`, `COL`, `dOut`, `dMed`, or `dCol`.

## Errors and edge cases

- If exposure/outcome cannot be inferred uniquely, the function stops
  with a clear message.

- Fitting errors (e.g., FE collinearity) are captured and displayed in
  comparisons without aborting the whole pipeline.

## See also

[`print.DAGassist_report()`](https://grahamgoff.github.io/DAGassist/reference/print.DAGassist_report.md)
for the console printer, and the helper exporters in `report_*` modules.

## Examples

``` r
if (requireNamespace("dagitty", quietly = TRUE)) {
  g <- dagitty::dagitty("dag { Z -> X; X -> M; X -> Y; M -> Y; Z -> Y }")
  dagitty::exposures(g) <- "X"; dagitty::outcomes(g) <- "Y"
  n <- 300
  Z <- rnorm(n); X <- 0.8*Z + rnorm(n)
  M <- 0.9*X + rnorm(n)
  Y <- 0.7*X + 0.6*M + 0.3*Z + rnorm(n)
  df <- data.frame(Z, X, M, Y)

  # 1) Core: DAG-derived specs + engine-call parsing
  r <- DAGassist(g, lm(Y ~ X + Z + M, data = df))

  # 2) Target sample-average estimands via weighting
  r2 <- DAGassist(g, lm(Y ~ X + Z + M, data = df), estimand = "SATE")

  # 3) Mediator case: Raw sequential-g vs Weighted SACDE
  r3 <- DAGassist(g, lm(Y ~ X + Z + M, data = df), estimand = "SACDE")

  # 4) File export (LaTeX fragment)
  # \donttest{
    out <- file.path(tempdir(), "dagassist_report.tex")
    DAGassist(g, lm(Y ~ X + Z + M, data = df), type = "latex", out = out)
  # }
}
```
