# DAGassist 0.2.3

## Highlights:

- Fixed `imply` logic. Previously, for the roles table, `imply = FALSE` would only display RHS variables, but still assessed all DAG-based relationships.
- Renamed `desc(Y)` `IO`, for `intermediate outcome`
- Dropped `desc(X)` as a roles column
- Added `dMed` (descendant of mediator) and `dCol` (descendant of collider)
- Updated documentation and added vignettes

This is the last release before CRAN sumbission. 

# DAGassist 0.2.2

## Highlights:

- Built **pkgdown** site and several supporting vignettes.
- Significantly enhanced package documentation in preparation for CRAN release.
- Trimmed exported functions.
- Enhanced and documented engine compatibility.
- Added `omit_factors` and `omit_intercept` parameters.
- Reworked `imply` parameter so it restricts evaluation to variables listed in RHS.
- Fixed cluster argument bug.

# DAGassist 0.2.1 

## Added
- **variable labels** across roles + model tables (LaTeX, .docx, .xlsx, .txt).
- **unevaluated regressors** note: list RHS terms not in the DAG, shown in all outputs.
- **LaTeX + Word + Excel + Text**:
  - keep factor dummies and fixed effects by default.
  - better `longtblr` conversion/spacing; improved dynamic column width weighting and line breaking for long labels.
  - auto-escape labels for neat output.
  - cleaner summaries and label support.
## Changed
  - consolidated helpers (`assist_helpers.R` + `export_helpers.R`).
  - improved handling of nuisance vars (FEs/tails).
  - significantly improved documentation to prep for CRAN release. 
  - broader validation against real replication data.

# DAGassist 0.2.0

## Breaking changes
- Renamed the main entry point from `dag_assist()` to `DAGassist()`.  

## Added
- Acyclicity checks and clearer validation messages in `validate.R`.
- Canonical adjustment sets alongside minimal sets.
- Support for multiple minimal adjustment sets.
- Export to **LaTeX**, **Word (.docx)**, **Excel (.xlsx)**, and **plain text (.txt)**.
- `export_report()` helper to produce a publication-grade, half-page robustness report from a single call.
- New `imply` parameter to control DAG-implied covariate additions  

## Changed
- Console report redesigned with compact, readable tables 
- Nicer console styling with color

## Notes
- Fixed-effects / IV parts of formulas (e.g., `| region + time`) are preserved; validation looks only at the pre-`|` part.