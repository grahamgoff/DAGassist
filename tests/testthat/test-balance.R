# tests/testthat/test-balance.R

test_that(".dagassist_smd: binary uses a raw mean difference", {
  # mean(ref) = 0.5, mean(cmp) = 0.25 -> raw diff = 0.25
  expect_equal(
    .dagassist_smd(c(1, 1, 0, 0), c(1, 0, 0, 0), binary = TRUE),
    0.25
  )
})

test_that(".dagassist_smd: continuous standardizes by pooled SD", {
  # ref = 1:5 (mean 3, var 2.5); cmp = 2:6 (mean 4, var 2.5)
  # denom = sqrt((2.5 + 2.5)/2) = sqrt(2.5)
  expect_equal(
    .dagassist_smd(1:5, 2:6),
    (3 - 4) / sqrt(2.5),
    tolerance = 1e-8
  )
})

test_that(".dagassist_smd: degenerate cases are handled", {
  # identical constants -> 0 (not NaN)
  expect_equal(.dagassist_smd(c(2, 2, 2), c(2, 2, 2)), 0)
  # constant but mismatched means -> Inf (always flagged)
  expect_true(is.infinite(.dagassist_smd(c(1, 1, 1), c(2, 2, 2))))
  # too few observations -> NA
  expect_true(is.na(.dagassist_smd(numeric(0), 1:5)))
})

test_that(".dagassist_balance_kind: classifies variable types", {
  expect_identical(.dagassist_balance_kind(c(0, 1, 1, 0)),            "binary")
  expect_identical(.dagassist_balance_kind(c(TRUE, FALSE, TRUE)),     "binary")
  expect_identical(.dagassist_balance_kind(factor(c("a", "b", "a"))), "binary")
  expect_identical(.dagassist_balance_kind(rnorm(30)),               "continuous")
  expect_identical(.dagassist_balance_kind(factor(c("a","b","c","a"))), "factor")
})

test_that(".dagassist_balance_compare: flags shifted covariates only", {
  set.seed(123)
  base <- rnorm(200)
  df <- data.frame(
    shift = c(rnorm(200, mean = 0), rnorm(200, mean = 2)),  # shifts across samples
    same  = c(base, base)                                    # identical across samples
  )
  ref <- c(rep(TRUE,  200), rep(FALSE, 200))
  cmp <- !ref
  
  bt <- .dagassist_balance_compare(df, ref, cmp, c("shift", "same"), threshold = 0.1)
  
  expect_true(bt$flagged[bt$variable == "shift"])
  expect_false(bt$flagged[bt$variable == "same"])
})

test_that(".dagassist_balance_compare: expands multi-level factors to dummies", {
  df  <- data.frame(grp = factor(rep(c("a", "b", "c"), each = 40)))
  ref <- rep(c(TRUE, FALSE), length.out = 120)
  bt  <- .dagassist_balance_compare(df, ref, !ref, "grp", threshold = 0.1)
  
  # one row per level, each treated as binary
  expect_setequal(bt$variable, c("grp [a]", "grp [b]", "grp [c]"))
  expect_true(all(bt$type == "binary"))
})

test_that("balance diagnostics print and flag a shifted listwise-deletion sample", {
  skip_if_no_dagitty()
  
  set.seed(42)
  n  <- 4000
  Z  <- rnorm(n)                                    # confounder
  X  <- rbinom(n, 1, plogis(0.8 * Z))               # binary treatment
  Y  <- 1.0 * X + 0.7 * Z + rnorm(n)
  C  <- 0.5 * Z + 0.5 * X + rnorm(n)                # collider / descendant of X
  miss <- rbinom(n, 1, plogis(-0.5 + 1.5 * X + 1.0 * Z)) == 1
  C[miss] <- NA                                     # missingness depends on X and Z
  dat <- data.frame(Y, X, Z, C)
  
  g <- dagitty::dagitty("dag { Z -> X; Z -> Y; X -> Y; Z -> C; X -> C }")
  rpt <- DAGassist(g, Y ~ X + Z + C, dat, exposure = "X", outcome = "Y")
  
  # Original (requires C) fits on a biased subsample; minimal/canonical drop C
  # and use the full data, so Z's composition shifts -> at least one flag.
  expect_output(.dagassist_print_balance_diagnostics(rpt), "Balance diagnostics")
  expect_output(.dagassist_print_balance_diagnostics(rpt), "imbalanced")
  
  # and it is wired into the report printer
  expect_output(print(rpt), "Balance diagnostics")
})

test_that("balance diagnostics are quiet when there is no data", {
  skip_if_no_dagitty()
  d   <- make_dag_confounder()
  df  <- sim_data_confounder(seed = 7)
  rpt <- DAGassist(d, Y ~ X + Z, df, "X", "Y")
  rpt$.__data <- NULL                               # simulate a data-less report
  expect_silent(.dagassist_print_balance_diagnostics(rpt))
})