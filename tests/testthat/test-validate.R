test_that("errors when dag is not a dagitty object", {
  df <- data.frame(Y = 1:2, X = 1:2)
  expect_error(
    validate_spec(dag = list(), formula = Y ~ X, data = df, exposure = "X", outcome = "Y"),
    regexp = "dagitty object", fixed = TRUE
  )
})

test_that("happy path: valid DAG + data returns ok = TRUE with no issues", {
  skip_if_not_installed("dagitty")
  d  <- dagitty::dagitty("dag { U -> X; U -> Y; X -> Y }")
  set.seed(1)
  n  <- 20
  U  <- rnorm(n); X <- 0.7*U + rnorm(n); Y <- 1*X + 0.5*U + rnorm(n)
  df <- data.frame(Y, X, U)
  
  v <- validate_spec(dag = d, formula = Y ~ X + U, data = df, exposure = "X", outcome = "Y")
  expect_true(v$ok)
  expect_equal(nrow(v$issues), 0L)
})

test_that("non-DAG nuisance variables on RHS are allowed (no error)", {
  skip_if_not_installed("dagitty")
  d  <- dagitty::dagitty("dag { U -> X; U -> Y; X -> Y }")  # no Z in DAG
  df <- data.frame(Y = rnorm(10), X = rnorm(10), U = rnorm(10), Z = rnorm(10))
  
  v <- validate_spec(dag = d, formula = Y ~ X + Z, data = df, exposure = "X", outcome = "Y")
  
  # With the new policy, this should be OK (no error about Z missing in DAG)
  expect_true(v$ok)
  expect_false(any(v$issues$type == "missing_in_dag" & v$issues$variable == "Z"))
})

test_that("variable in formula but not in data triggers missing_in_data", {
  skip_if_not_installed("dagitty")
  d  <- dagitty::dagitty("dag { U -> X; U -> Y; X -> Y }")
  df <- data.frame(Y = rnorm(10), X = rnorm(10))  # U missing
  
  v <- validate_spec(d = d, formula = Y ~ X + U, data = df, exposure = "X", outcome = "Y")
  expect_false(v$ok)
  expect_true(any(v$issues$type == "missing_in_data" & v$issues$variable == "U"))
})

test_that("exposure/outcome must be in DAG (still an error)", {
  skip_if_not_installed("dagitty")
  d  <- dagitty::dagitty("dag { U -> X; U -> Y; X -> Y }")
  df <- data.frame(Y = rnorm(10), X = rnorm(10), U = rnorm(10))
  
  # Exposure not in DAG
  v1 <- validate_spec(dag = d, formula = Y ~ X + U, data = df, exposure = "X_not_in_dag", outcome = "Y")
  expect_false(v1$ok)
  expect_true(any(v1$issues$type == "missing_in_dag" & v1$issues$variable == "X_not_in_dag"))
  
  # Outcome not in DAG
  v2 <- validate_spec(dag = d, formula = Y ~ X + U, data = df, exposure = "X", outcome = "Y_not_in_dag")
  expect_false(v2$ok)
  expect_true(any(v2$issues$type == "missing_in_dag" & v2$issues$variable == "Y_not_in_dag"))
})
