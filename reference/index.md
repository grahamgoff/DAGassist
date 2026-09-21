# Package index

## Main function

One call validates the DAG, classifies roles, fits comparison models,
and renders the report.

- [`DAGassist()`](https://grahamgoff.com/DAGassist/reference/DAGassist.md)
  : Generate and/or export report that classifies nodes, compares
  models, and (optionally) target causal estimands.

## Robustness to DAG uncertainty

Check whether conclusions survive uncertain edge directions or plausibly
missing arrows.

- [`pdag_robustness()`](https://grahamgoff.com/DAGassist/reference/pdag_robustness.md)
  : Diagnose adjustment-set and role robustness to uncertain edge
  directions
- [`add_edges_robustness()`](https://grahamgoff.com/DAGassist/reference/add_edges_robustness.md)
  : Diagnose robustness to added ("missing") edges

## Diagnostics and building blocks

Standalone helpers used inside DAGassist() that are also useful on their
own.

- [`classify_nodes()`](https://grahamgoff.com/DAGassist/reference/classify_nodes.md)
  : Classify DAG nodes
- [`bad_controls_in()`](https://grahamgoff.com/DAGassist/reference/bad_controls_in.md)
  : flag bad controls (mediator/collider/desc of Y) among a candidate
  set
- [`balance_models()`](https://grahamgoff.com/DAGassist/reference/balance_models.md)
  : Balance diagnostics across a list of fitted models

## Data

Simulated datasets and their generating DAGs, used throughout the
documentation and examples.

- [`toy_data`](https://grahamgoff.com/DAGassist/reference/toy_data.md) :
  Toy data for demonstrating causal roles
- [`toy_dag`](https://grahamgoff.com/DAGassist/reference/toy_dag.md) :
  DAG behind toy_data
- [`turnout_data`](https://grahamgoff.com/DAGassist/reference/turnout_data.md)
  : Simulated data on income and voter turnout
- [`turnout_dag`](https://grahamgoff.com/DAGassist/reference/turnout_dag.md)
  : DAG behind turnout_data

## Print methods

Console printers for DAGassist objects. You rarely call these directly.

- [`print(`*`<DAGassist_addedge_summary>`*`)`](https://grahamgoff.com/DAGassist/reference/print.DAGassist_addedge_summary.md)
  : Print an edge-addition (exclusion) robustness summary
- [`print(`*`<DAGassist_pdag_summary>`*`)`](https://grahamgoff.com/DAGassist/reference/print.DAGassist_pdag_summary.md)
  : Print a PDAG robustness summary
- [`print(`*`<DAGassist_report>`*`)`](https://grahamgoff.com/DAGassist/reference/print.DAGassist_report.md)
  : Print method for DAGassist reports
- [`print(`*`<DAGassist_roles>`*`)`](https://grahamgoff.com/DAGassist/reference/print.DAGassist_roles.md)
  : Print node classifications (aligned)
- [`print(`*`<DAGassist_validation>`*`)`](https://grahamgoff.com/DAGassist/reference/print.DAGassist_validation.md)
  : Minimal, clean printout for validation results with color coding
