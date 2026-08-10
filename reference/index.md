# Package index

## Main function

The primary pipeline function to generate reports.

- [`DAGassist()`](https://grahamgoff.com/DAGassist/reference/DAGassist.md)
  : Generate and/or export report that classifies nodes, compares
  models, and (optionally) target causal estimands.

## Useful supplements

Helper functions that can be useful on their own.

- [`bad_controls_in()`](https://grahamgoff.com/DAGassist/reference/bad_controls_in.md)
  : flag bad controls (mediator/collider/desc of Y) among a candidate
  set
- [`classify_nodes()`](https://grahamgoff.com/DAGassist/reference/classify_nodes.md)
  : Classify DAG nodes
- [`pdag_robustness()`](https://grahamgoff.com/DAGassist/reference/pdag_robustness.md)
  : Diagnose adjustment-set and role robustness to uncertain edge
  directions
- [`add_edges_robustness()`](https://grahamgoff.com/DAGassist/reference/add_edges_robustness.md)
  : Diagnose robustness to added ("missing") edges
- [`balance_models()`](https://grahamgoff.com/DAGassist/reference/balance_models.md)
  : Balance diagnostics across a list of fitted models

## Internal functions

Lower-level helpers and print methods, usually not called directly.

- [`print(`*`<DAGassist_report>`*`)`](https://grahamgoff.com/DAGassist/reference/print.DAGassist_report.md)
  : Print method for DAGassist reports
- [`print(`*`<DAGassist_roles>`*`)`](https://grahamgoff.com/DAGassist/reference/print.DAGassist_roles.md)
  : Print node classifications (aligned)
- [`print(`*`<DAGassist_validation>`*`)`](https://grahamgoff.com/DAGassist/reference/print.DAGassist_validation.md)
  : Minimal, clean printout for validation results with color coding
- [`print(`*`<DAGassist_pdag_summary>`*`)`](https://grahamgoff.com/DAGassist/reference/print.DAGassist_pdag_summary.md)
  : Print a PDAG robustness summary
- [`print(`*`<DAGassist_addedge_summary>`*`)`](https://grahamgoff.com/DAGassist/reference/print.DAGassist_addedge_summary.md)
  : Print an edge-addition (exclusion) robustness summary
