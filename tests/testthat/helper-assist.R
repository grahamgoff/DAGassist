# tests/testthat/helper-assist.R
# Helpers for dag_assist / compare tests (loaded automatically by testthat)

# Skip if dagitty isn't installed (keeps CI happy)
skip_if_no_dagitty <- function() {
  testthat::skip_if_not_installed("dagitty")
}

# Classic confounder DAG: Z -> X, Z -> Y, X -> Y
make_dag_confounder <- function() {
  dagitty::dagitty("dag { Z -> X; Z -> Y; X -> Y }")
}

# DAG with confounder (Z), mediator (M), and collider (C) on X–Y path
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

# Collider off the X–Y path (should NOT be flagged)
make_dag_offpath_collider <- function() {
  dagitty::dagitty("dag { X [exposure]; Y [outcome]; A; B; C; X -> Y; A -> C; B -> C }")
}

# Simulate quick numeric data consistent with the confounder DAG
# Includes extra columns (A,B,C,M) so tests can reference them without error
sim_data_confounder <- function(n = 150, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  Z <- rnorm(n)
  X <- 0.9 * Z + rnorm(n, sd = 0.6)
  Y <- 1.2 * X + 0.5 * Z + rnorm(n, sd = 0.8)
  A <- rnorm(n); B <- rnorm(n); C <- rnorm(n); M <- rnorm(n)
  data.frame(Y, X, Z, A, B, C, M)
}

# Binary outcome version for glm tests
sim_data_confounder_binary <- function(n = 120, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  Z <- rnorm(n)
  X <- 0.9 * Z + rnorm(n, sd = 0.6)
  p <- plogis(-0.5 + 1.0 * X + 0.6 * Z)
  Y <- rbinom(n, 1, p)
  A <- rnorm(n); B <- rnorm(n); C <- rnorm(n); M <- rnorm(n)
  data.frame(Y, X, Z, A, B, C, M)
}