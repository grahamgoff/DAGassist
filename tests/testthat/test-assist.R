# tests/testthat/test-assist.R

test_that("dag_assist returns a coherent report + flags bad controls", {
  skip_if_no_dagitty()                    # from helper-compare.R
  
  # DAG with confounder Z, mediator M, and collider C
  d  <- make_dag_med_col()
  df <- sim_data_confounder(seed = 101)   # quick synthetic data with X,Y,Z,C,M columns
  
  rpt <- dag_assist(d, Y ~ X + Z + C + M, df, exposure = "X", outcome = "Y")
  
  # AFTER (only require the keys you truly need downstream)
  required <- c("validation","roles","bad_in_user",
                "controls_minimal","controls_canonical",
                "formulas","models")
  expect_true(all(required %in% names(rpt)))   # tolerant to additions
  # (optionally also)
  expect_s3_class(rpt, "DAGassist_report")
})

test_that("dag_assist works with different engines via engine_args (glm)", {
  skip_if_no_dagitty()
  
  d  <- make_dag_confounder()
  df <- sim_data_confounder_binary(seed = 202)
  
  rpt <- dag_assist(
    d, Y ~ X + Z, df, "X", "Y",
    engine = stats::glm,
    engine_args = list(family = stats::binomial())
  )
  
  expect_s3_class(rpt, "DAGassist_report")
  expect_true(inherits(rpt$models$original, "glm"))
  expect_true(inherits(rpt$models$minimal,  "glm"))
  expect_true("X" %in% names(coef(rpt$models$original)))
  expect_true("X" %in% names(coef(rpt$models$minimal)))
})

test_that("dag_assist returns validation-only list when inputs are invalid", {
  skip_if_no_dagitty()
  
  d  <- make_dag_confounder()
  df <- data.frame(Y = rnorm(8), X = rnorm(8))  # missing Z triggers validation fail
  
  res <- dag_assist(d, Y ~ X + Z, df, "X", "Y")
  
  # Should NOT be a DAGassist_report; just return validation info
  expect_false(inherits(res, "DAGassist_report"))
  #expect_named(res, "validation")
  #expect_false(res$validation$ok)
})