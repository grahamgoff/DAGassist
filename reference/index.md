# Package index

## Main function

The primary pipeline function to generate reports.

- [`DAGassist()`](https://grahamgoff.github.io/DAGassist/reference/DAGassist.md)
  : Generate a (console/LaTeX/word/excel/txt) report classifying nodes
  and comparing models

## Useful supplements

Helper functions that can be useful on their own.

- [`bad_controls_in()`](https://grahamgoff.github.io/DAGassist/reference/bad_controls_in.md)
  : flag bad controls (mediator/collider/desc of Y) among a candidate
  set
- [`classify_nodes()`](https://grahamgoff.github.io/DAGassist/reference/classify_nodes.md)
  : Classify DAG nodes

## Internal functions

Lower-level helpers and print methods, usually not called directly.

- [`print(`*`<DAGassist_report>`*`)`](https://grahamgoff.github.io/DAGassist/reference/print.DAGassist_report.md)
  : Print method for DAGassist reports
- [`print(`*`<DAGassist_roles>`*`)`](https://grahamgoff.github.io/DAGassist/reference/print.DAGassist_roles.md)
  : Print node classifications (aligned)
- [`print(`*`<DAGassist_validation>`*`)`](https://grahamgoff.github.io/DAGassist/reference/print.DAGassist_validation.md)
  : Minimal, clean printout for validation results with color coding
