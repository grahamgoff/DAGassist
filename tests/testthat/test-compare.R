##set up the helper functions

# make sure dagity is installed
skip_if_no_dagitty <- function() {
  testthat::skip_if_not_installed("dagitty")
}

# confounder
make_dag_confounder <- function() {
  dagitty::dagitty("dag { Z -> X; Z -> Y; X -> Y }")
}

# mediator + collider
make_dag_med_col <- function() {
  dagitty::dagitty("
    dag {
      X [exposure]; Y [outcome]; Z; M; C
      Z -> X; Z -> Y; X -> Y
      X -> M; M -> Y
      X -> C; Y -> C
    }
  ")
}

# collider not on x/y path
make_dag_offpath_collider <- function() {
  dagitty::dagitty("dag { X [exposure]; Y [outcome]; A; B; C; X -> Y; A -> C; B -> C }")
}

# Simulate data for lm
sim_data_confounder <- function(n = 150, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  Z <- rnorm(n)
  X <- 0.9 * Z + rnorm(n, sd = 0.6)
  Y <- 1.2 * X + 0.5 * Z + rnorm(n, sd = 0.8)
  A <- rnorm(n)
  B <- rnorm(n)
  C <- rnorm(n)
  M <- rnorm(n)
  
  data.frame(Y, X, Z, A, B, C, M)
}

# Simulate binary outcome for glm 
sim_data_confounder_binary <- function(n = 120, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  Z <- rnorm(n)
  X <- 0.9 * Z + rnorm(n, sd = 0.6)
  p <- plogis(-0.5 + 1.0 * X + 0.6 * Z)
  Y <- rbinom(n, 1, p)
  A <- rnorm(n)
  B <- rnorm(n)
  C <- rnorm(n)
  M <- rnorm(n)
  
  data.frame(Y, X, Z, A, B, C, M)
}

# Compare formulas by their printed form (ignores environment attributes)
expect_formula_text <- function(f, txt) {
  testthat::expect_true(inherits(f, "formula"))
  testthat::expect_equal(deparse(f), txt)
}

# Or compare two formulas directly, ignoring environments
expect_formula_same <- function(a, b) {
  testthat::expect_equal(deparse(a), deparse(b))
}

test_that("Minimal controls for classic confounding are just {Z}", {
  skip_if_no_dagitty()
  
  d  <- make_dag_confounder()
  cs <- pick_minimal_controls(d, exposure = "X", outcome = "Y")
  
  expect_equal(cs, "Z")  # only the confounder Z is needed
})

test_that("Bad controls = mediator/collider (not confounder)", {
  skip_if_no_dagitty()
  
  # Z is good (confounder), M is bad (mediator), C is bad (collider)
  d <- make_dag_med_col()
  
  bad <- bad_controls_in(d, controls = c("Z", "M", "C"),
                         exposure = "X", outcome = "Y")
  
  expect_setequal(bad, c("M", "C"))
  
  # Off-path collider should NOT be flagged
  d_off <- make_dag_offpath_collider()
  bad_off <- bad_controls_in(d_off, controls = "C",
                             exposure = "X", outcome = "Y")
  expect_length(bad_off, 0)
})

test_that("update_to_controls builds a clean formula", {
  f <- update_to_controls(exposure = "X", outcome = "Y", controls = c("Z", "W"))
  expect_true(inherits(f, "formula"))
  expect_formula_text(f, "Y ~ X + Z + W")
})

test_that("compare_specs: structure, minimal set, formulas, and lm fits", {
  skip_if_no_dagitty()
  df <- sim_data_confounder(seed = 123)
  d  <- make_dag_confounder()
  
  cmp <- compare_specs(d, Y ~ X + Z, df, exposure = "X", outcome = "Y")
  
  # object shape
  expect_s3_class(cmp, "DAGassist_compare")
  expect_named(cmp, c("validation", "original", "minimal", "controls", "formulas"),
               ignore.order = TRUE)
  expect_true(cmp$validation$ok)
  
  # minimal set and formulas
  expect_equal(cmp$controls, "Z")
  expect_formula_text(cmp$formulas$original, "Y ~ X + Z")
  expect_formula_text(cmp$formulas$minimal, "Y ~ X + Z")
  
  # fits look like lm objects; coef for X exists
  expect_true(inherits(cmp$original, "lm"))
  expect_true(inherits(cmp$minimal,  "lm"))
  expect_true("X" %in% names(coef(cmp$original)))
  expect_true("X" %in% names(coef(cmp$minimal)))
})

test_that("compare_specs works with glm via engine_args", {
  skip_if_no_dagitty()
  df <- sim_data_confounder_binary(seed = 456)
  d  <- make_dag_confounder()
  
  cmp <- compare_specs(
    d, Y ~ X + Z, df, "X", "Y",
    engine = stats::glm,
    engine_args = list(family = stats::binomial())
  )
  
  expect_s3_class(cmp, "DAGassist_compare")
  expect_true(inherits(cmp$original, "glm"))
  expect_true(inherits(cmp$minimal,  "glm"))
  expect_true("X" %in% names(coef(cmp$original)))
  expect_true("X" %in% names(coef(cmp$minimal)))
})

test_that("compare_specs returns validation-only list when inputs are invalid", {
  skip_if_no_dagitty()
  d  <- make_dag_confounder()
  df <- data.frame(Y = rnorm(10), X = rnorm(10))  # missing Z
  
  res <- compare_specs(d, Y ~ X + Z, df, "X", "Y")
  
  expect_false(inherits(res, "DAGassist_compare"))  # not classed
  expect_named(res, "validation")
  expect_false(res$validation$ok)
})

test_that("print.DAGassist_compare emits a concise report", {
  skip_if_no_dagitty()
  df <- sim_data_confounder(seed = 789)
  d  <- make_dag_confounder()
  
  cmp <- compare_specs(d, Y ~ X + Z, df, "X", "Y")
  expect_output(print(cmp), "DAGassist compare")
  expect_output(print(cmp), "Validation: VALID")
  expect_output(print(cmp), "Controls \\(minimal\\): \\{Z\\}")
})