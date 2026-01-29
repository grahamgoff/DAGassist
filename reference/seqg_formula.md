# Build a DirectEffects::sequential_g() formula from a DAG (and optionally a base model formula)

Returns a formula of the form `Y ~ A + X | Z | M` required by
[`DirectEffects::sequential_g()`](https://mattblackwell.github.io/DirectEffects/reference/sequential_g.html),
where:

- `A` is the treatment (exposure),

- `X` are baseline covariates,

- `Z` are intermediate (post-treatment) covariates affected by A that
  affect both M and Y,

- `M` are mediators (blip-down model terms).

If `formula` is provided, `X` is (by default) constrained to the RHS
terms that appear in that base formula (mirroring DAGassist's current
ACDE construction).

## Usage

``` r
seqg_formula(
  dag,
  formula = NULL,
  data = NULL,
  exposure = NULL,
  outcome = NULL,
  m = NULL,
  x = NULL,
  z = NULL,
  include_descendants = FALSE,
  fe = NULL,
  fe_as_factor = TRUE,
  drop_fe_collinear = TRUE,
  strict_rhs = TRUE,
  return_parts = TRUE
)
```

## Arguments

- dag:

  A `dagitty` object (or a character DAGitty string).

- formula:

  Optional base formula (or an engine call like `fixest::feols(...)`).
  If supplied, the function uses its RHS to define candidate covariates
  and to pick up fixed-effects/grouping variables (which can be added to
  X).

- data:

  Optional data.frame. Used to (i) drop terms that do not exist in the
  data, and (ii) optionally drop covariates collinear with fixed effects
  (important for seqg stability).

- exposure, outcome:

  Optional. If missing, inferred from
  [`dagitty::exposures()`](https://rdrr.io/pkg/dagitty/man/VariableStatus.html)
  /
  [`dagitty::outcomes()`](https://rdrr.io/pkg/dagitty/man/VariableStatus.html).

- m, x, z:

  Optional character vectors overriding inferred mediator (M), baseline
  (X), and intermediate (Z) terms.

- include_descendants:

  Logical; if TRUE, allow descendants of mediators to be included in M
  (conservatively restricted to descendants that remain ancestors of Y
  and descendants of A).

- fe:

  Optional character vector of FE/grouping variables to include in X
  (merged with any extracted from `formula`).

- fe_as_factor:

  Logical; if TRUE, wraps bare FE names as `factor(FE)` (only when FE
  exists in `data`).

- drop_fe_collinear:

  Logical; if TRUE and `data` is provided, drop any single-variable term
  that is constant within an FE group (DirectEffects is less forgiving
  than fixest here).

- strict_rhs:

  Logical; if TRUE and `formula` is provided, X is drawn only from RHS
  terms in `formula`. If FALSE, X is drawn from the DAG node set
  (pre-treatment ancestors) even if not in `formula`.

- return_parts:

  Logical; if TRUE, returns a list with components and metadata;
  otherwise returns just the formula.

## Value

A formula (or a list with `$formula`, `$x`, `$z`, `$m`, `$fe`,
`$dropped_fe`).
