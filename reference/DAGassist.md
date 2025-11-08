# Generate a (console/LaTeX/word/excel/txt) report classifying nodes and comparing models

`DAGassist()` validates a DAG + model specification, classifies node
roles, builds minimal and canonical adjustment sets, fits comparable
models, and renders a compact report in several formats (console, LaTeX
fragment, DOCX, XLSX, plain text). It also supports passing a **single
engine call** (e.g. `feols(Y ~ X + Z | fe, data = df)`) instead of a
plain formula.

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
  engine_args = list()
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
  `"latex"`/`"docx"`/`"word"`, `"excel"`/`"xlsx"`, `"text"`/`"txt"`.

- show:

  Which sections to include in the output. One of `"all"` (default),
  `"roles"`, or `"models"`.

- out:

  Output file path for the non-console types:

  - `type="latex"`: a **LaTeX fragment** written to `out` (must end with
    `.tex`).

  - `type="docx"`/`"word"`: a **Word (.docx)** file written to `out`.

  - `type="excel"`/`"xlsx"`: an **Excel (.xlsx)** file written to `out`.

  - `type="text"`/`"txt"`: a **plain-text** file written to `out`.
    Ignored for `type="console"`.

- imply:

  Logical; default `FALSE`. **Evaluation scope.**

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

- omit_intercept:

  Logical; drop intercept rows from the model comparison display
  (default `TRUE`).

- omit_factors:

  Logical; drop factor-level rows from the model comparison display
  (default `TRUE`). This parameter only suppresses factor
  **output**–they are still included in the regression.

- bivariate:

  if TRUE, include a bivariate (no covariate) model in the comparison
  table. Default FALSE.

- engine_args:

  Named list of extra arguments forwarded to `engine(...)`. If `formula`
  is an engine call, arguments from the call are merged with
  `engine_args` (call values take precedence).

## Value

An object of class `"DAGassist_report"`, invisibly for file outputs, and
printed for `type="console"`. The list contains:

- `validation` - result from `validate_spec(...)` which verifies
  acyclicity and X/Y declarations.

- `roles` - raw roles data.frame from `classify_nodes(...)` (logic
  columns).

- `roles_display` - roles grid after labeling/renaming for exporters.

- `bad_in_user` - variables in the user's RHS that are
  `MED`/`COL`/`dOut`/`dMed`/`dCol`.

- `controls_minimal` - (legacy) one minimal set (character vector).

- `controls_minimal_all` - list of all minimal sets (character vectors).

- `controls_canonical` - canonical set (character vector; may be empty).

- `formulas` - list with `original`, `minimal`, `minimal_list`,
  `canonical`.

- `models` - list with fitted models `original`, `minimal`,
  `minimal_list`, `canonical`.

- `verbose`, `imply` - flags as provided.

## Details

**Engine-call parsing.** If `formula` is a call (e.g.,
`feols(Y ~ X | fe, data=df)`), DAGassist extracts the engine function,
formula, data argument, and any additional engine arguments directly
from that call; these are merged with `engine`/`engine_args` you pass
explicitly (call arguments win).

**Fixest tails.** For engines like **fixest** that use `|` to denote
FE/IV parts, DAGassist preserves any `| ...` tail when constructing
minimal/canonical formulas (e.g., `Y ~ X + controls | fe | iv(...)`).

**Roles grid.** The roles table displays short headers:

- `X` (exposure), `Y` (outcome), `CON` (confounder), `MED` (mediator),
  `COL` (collider), `dOut` (proper descendant of `Y`), `dMed` (proper
  descendant of any mediator), `dCol` (proper descendant of any
  collider). Descendants are **proper** (exclude the node itself) and
  can be any distance downstream. The internal
  `is_descendant_of_exposure` is retained for logic but hidden in
  displays.

**Bad controls.** For total-effect estimation, DAGassist flags as
`bad controls` any variables that are `MED`, `COL`, `dOut`, `dMed`, or
`dCol`. These are warned in the console and omitted from the
model-comparison table. Valid confounders (pre-treatment) are eligible
for minimal/canonical adjustment sets.

**Output types.**

- `console` prints roles, sets, formulas (if `verbose`), and a compact
  model comparison with `{modelsummary}` if available (falls back
  gracefully otherwise).

- `latex` writes a **LaTeX fragment** you can `\\input{}` into a paper.

- `docx`/`word` writes a **Word** doc (uses
  `options(DAGassist.ref_docx=...)` if set).

- `excel`/`xlsx` writes an **Excel** workbook with tidy tables.

- `text`/`txt` writes a **plain-text** report for logs/notes.

**Dependencies.** Core requires `{dagitty}`. Optional enhancements:
`{modelsummary}` (pretty tables), `{broom}` (fallback tidying),
`{rmarkdown}` + **Pandoc** (DOCX), `{writexl}` (XLSX).

## Interpreting the output

**Roles:** Variables in your formula are classified by DAG-based causal
role:

- `X` - treatment / exposure.

- `Y` - outcome / dependent variable.

- `CON` - confounder (common cause of `X` and `Y`); adjust for these.

- `MED` - mediator (on a path from `X` to `Y`); do **not** adjust when
  estimating total effects.

- `COL` - collider (direct descendant of `X` and `Y`); adjusting opens a
  spurious path, so do **not** adjust.

- `dOut` - descendant of outcome; do **not** adjust.

- `dMed` - descendant of a mediator; do **not** adjust when estimating
  total effects.

- `dCol` - descendant of a collider; adjusting opens a spurious path, so
  do **not** adjust.

- `other` - safe, non-confounding predictors (e.g., affect `Y` only).
  Included in the canonical model but omitted from the minimal set
  because they're not required for identification.

**Model Comparison:**

- **Minimal** - the smallest adjustment set that blocks all back-door
  paths (confounders only).

- **Canonical** - the largest permissible set: includes all controls
  that are not `MED`, `COL`, `dOut`, `dMed`, or `dCol`. `other`
  variables may appear here.

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
# generate a console DAGassist report
DAGassist(dag = g, formula = lm(Y ~ X + Z + C + M, data = df))
#> DAGassist Report: 
#> 
#> Roles:
#> variable  role        X  Y  conf  med  col  dOut  dMed  dCol  dConfOn  dConfOff  NCT  NCO
#> X         exposure    x                                                                  
#> Y         outcome        x                                                               
#> Z         confounder        x                                                            
#> M         mediator                x                                                      
#> C         collider                     x    x     x                                      
#> 
#>  (!) Bad controls in your formula: {C, M}
#> Minimal controls 1: {Z}
#> Canonical controls: {Z}
#> 
#> Formulas:
#>   original:  Y ~ X + Z + C + M
#> 
#> Model comparison:
#> 
#> +---+----------+-----------+-----------+
#> |   | Original | Minimal 1 | Canonical |
#> +===+==========+===========+===========+
#> | X | 0.467*** | 1.306***  | 1.306***  |
#> +---+----------+-----------+-----------+
#> |   | (0.122)  | (0.098)   | (0.098)   |
#> +---+----------+-----------+-----------+
#> | Z | 0.185+   | 0.235+    | 0.235+    |
#> +---+----------+-----------+-----------+
#> |   | (0.102)  | (0.127)   | (0.127)   |
#> +---+----------+-----------+-----------+
#> | C | 0.368*** |           |           |
#> +---+----------+-----------+-----------+
#> |   | (0.076)  |           |           |
#> +---+----------+-----------+-----------+
#> | M | 0.512*** |           |           |
#> +---+----------+-----------+-----------+
#> |   | (0.077)  |           |           |
#> +===+==========+===========+===========+
#> | + p < 0.1, * p < 0.05, ** p < 0.01,  |
#> | *** p < 0.001                        |
#> +===+==========+===========+===========+ 

# generate a LaTeX DAGassist report
# \donttest{
DAGassist(dag = g, formula = lm(Y ~ X + Z + C + M, data = df),
          type = "latex", out = file.path(tempdir(), "frag.tex"))
# }
# generate just the roles table in the console
DAGassist(dag = g, show = "roles")
#> DAGassist Report: 
#> 
#> Roles:
#> variable  role        X  Y  conf  med  col  dOut  dMed  dCol  dConfOn  dConfOff  NCT  NCO
#> X         exposure    x                                                                  
#> Y         outcome        x                                                               
#> Z         confounder        x                                                            
#> M         mediator                x                                                      
#> C         collider                     x    x     x                                      
#> A         nco                                                                         x  
#> B         nco                                                                         x  
```
