# DAGassist in the R DAG Ecosystem

## Introduction

This article explains the broader R DAG ecosystem and positions
**DAGassist** within it. It also explains the dependencies necessary to
maximize **DAGassist**’s functionality.

- **`dagitty`** for DAG specification and reasoning
- **`ggdag`** to visualize DAGs
- **`modelsummary`** to format regression tables
- **`broom`** to tidy models (coefs & goodness-of-fit)
- **`writexl`** to export multi‑sheet Excel workbooks
- **`rmarkdown`** to render shareable docs (HTML/Word/PDF) that embed
  DAGassist outputs

### The R DAG Ecosystem at a Glance

### The Differences Between `dagitty`, `ggdag`, and `DAGassist`

#### `dagitty`

- The core DAG toolkit for R. Creates the actual DAG object that `ggdag`
  and `DAGassist` use.
- Supports causal queries:
  - [`dagitty::adjustmentSets()`](https://rdrr.io/pkg/dagitty/man/adjustmentSets.html)
    scans a `dagitty` object to identify adjustment sets.
  - [`dagitty::isAcyclic()`](https://rdrr.io/pkg/dagitty/man/isAcyclic.html)
    verifies acyclicity.
  - [`dagitty::findCycle()`](https://rdrr.io/pkg/dagitty/man/isAcyclic.html)
    identifies cyclical variable relationships.

**Making a DAG with `dagitty`:**

``` r
#via dagitty
library(dagitty)

dag_model <- dagitty('dag {
    EffectiveN_electoral
    Gov_effectiveness_0_5
    LR_Polarization_perceived
    PID_noleaners [exposure]
    Presidentialsystem
    VoterAPI_Party [outcome]
    Year_0
    EffectiveN_electoral -> VoterAPI_Party
    Gov_effectiveness_0_5 -> VoterAPI_Party
    LR_Polarization_perceived -> VoterAPI_Party
    PID_noleaners -> LR_Polarization_perceived
    PID_noleaners -> VoterAPI_Party
    Presidentialsystem -> EffectiveN_electoral
    Presidentialsystem -> VoterAPI_Party
    Year_0 -> PID_noleaners
    Year_0 -> VoterAPI_Party
}')  
```

#### `ggdag`

- A visualization layer for DAGs.
- Also provides a more intuitive way to make `dagitty` objects.

**Making an identical DAG with `ggdag`:**

``` r
#via ggdag
library(ggdag)

dag_model <- dagify(
  VoterAPI_Party  ~ PID_noleaners + LR_Polarization_perceived +
    Gov_effectiveness_0_5 + EffectiveN_electoral + Presidentialsystem,
    
  LR_Polarization_perceived ~  PID_noleaners,
  
  EffectiveN_electoral ~ Presidentialsystem,
    
  exposure = "PID_noleaners",
  outcome = "VoterAPI_Party"
)
```

#### `DAGassist`

- The glue between your DAG models and regressions.
- Uses `dagitty` logic to classify the causal relationships embedded in
  a DAG.
- Identifies those relationships and their implications for your
  covariate selection in a concise, publication-grade report.

It uses `dagitty` logic to derive minimal and canonical adjustment sets,
according to these criteria:

| Path / Node Type                                 | Minimal | Canonical |
|--------------------------------------------------|:-------:|:---------:|
| Fork/Common–Cause Confounder (Z)                 |    ✓    |     ✓     |
| Chain/Mediator (M)                               |    ✗    |     ✗     |
| Collider (C)                                     |    ✗    |     ✗     |
| Descendant of Mediator (N)                       |    ✗    |     ✗     |
| Descendant of Collider (Q)                       |    ✗    |     ✗     |
| Descendant of Outcome (I)                        |    ✗    |     ✗     |
| M-Bias                                           |    ✗    |     ✗     |
| Butterfly Bias                                   |    ✗    |     ✗     |
| Neutral Control on Treatment (E → X)             |    ✗    |     ✓     |
| Neutral Control on Outcome (F → Y)               |    ✗    |     ✓     |
| Descendant of Confounder *off* Backdoor Path (W) |    ✗    |     ✗     |
| Descendant of Confounder *on* Backdoor Path (V)  | Z or V  |  Z and V  |

*Note:* ✓ = adjust; ✗ = do not adjust. There may be multiple minimal
sets; the canonical set is unique.
