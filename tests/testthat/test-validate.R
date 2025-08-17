test_that("errors when dag is not a dagitty object", {
  df <- data.frame(Y = 1:2, X = 1:2)
  # pass a wrong type for `dag`
  expect_error(
    validate_spec(dag = list(), formula = Y ~ X, data = df, exposure = "X", outcome = "Y"),
    "dagitty object"  # substring match; keeps it robust
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

test_that("variable in formula but not in DAG triggers missing_in_dag", {
  skip_if_not_installed("dagitty")
  d  <- dagitty::dagitty("dag { U -> X; U -> Y; X -> Y }")  # no Z node in DAG
  df <- data.frame(Y = rnorm(10), X = rnorm(10), U = rnorm(10), Z = rnorm(10))
  
  v <- validate_spec(dag = d, formula = Y ~ X + Z, data = df, exposure = "X", outcome = "Y")
  expect_false(v$ok)
  expect_true(any(v$issues$type == "missing_in_dag" & v$issues$variable == "Z"))
})

test_that("variable in formula but not in data triggers missing_in_data", {
  skip_if_not_installed("dagitty")
  d  <- dagitty::dagitty("dag { U -> X; U -> Y; X -> Y }")
  df <- data.frame(Y = rnorm(10), X = rnorm(10))  # U missing
  
  v <- validate_spec(dag = d, formula = Y ~ X + U, data = df, exposure = "X", outcome = "Y")
  expect_false(v$ok)
  expect_true(any(v$issues$type == "missing_in_data" & v$issues$variable == "U"))
})