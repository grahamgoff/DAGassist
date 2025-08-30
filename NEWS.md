# DAGassist NEWS

## Roadmap to v0.3.0:

- [ ] Ensure that classify.R categories are **always** accurate
- [ ] Verify compatibility with all major engines
- [ ] Handle diff-in-diff and fixed effects formula notation
- [ ] Add graphics: gray scale of the original model, with the minimal set visibly differentiated.
- [ ] Add a simple "covariate sensitivity" line to report, indicating if (i) the original model is statistically significant; (ii) the minimal or canonical set is statistically insignificant
- [ ] Run `dag_assist` on a bunch of complex DAGS to refine the sorting algorithm in `compare.R`.
- [ ] Get rid of the engine args param, which is obsolete

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