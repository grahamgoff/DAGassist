# tests/testthat/test-assist.R

test_that("dag_assist returns a coherent report + flags bad controls", {
  skip_if_no_dagitty()                    # from helper-compare.R
  
  # DAG with confounder Z, mediator M, and collider C
  d  <- make_dag_med_col()
  df <- sim_data_confounder(seed = 101)   # quick synthetic data with X,Y,Z,C,M columns
  
  rpt <- dag_assist(d, Y ~ X + Z + C + M, df, exposure = "X", outcome = "Y")
  
  # Shape/class
  expect_s3_class(rpt, "DAGassist_report")
  expect_named(rpt, c("validation","roles","bad_in_user",
                      "controls_minimal","formulas","models"),
               ignore.order = TRUE)
  expect_true(rpt$validation$ok)
  
  # “Bad controls” should be mediator & collider (not the confounder)
  expect_setequal(rpt$bad_in_user, c("M", "C"))
  
  # Minimal backdoor set for this graph is {Z}
  expect_equal(rpt$controls_minimal, "Z")
  
  # Formulas: original vs minimal (ignore env; compare text)
  expect_equal(deparse(rpt$formulas$original), "Y ~ X + Z + C + M")
  expect_equal(deparse(rpt$formulas$minimal),  "Y ~ X + Z")
  
  # Models exist and have X in the coefficients
  expect_true(inherits(rpt$models$original, "lm"))
  expect_true(inherits(rpt$models$minimal,  "lm"))
  expect_true("X" %in% names(coef(rpt$models$original)))
  expect_true("X" %in% names(coef(rpt$models$minimal)))
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
  expect_named(res, "validation")
  expect_false(res$validation$ok)
})

test_that("print.DAGassist_report emits a compact, readable summary", {
  skip_if_no_dagitty()
  
  d  <- make_dag_med_col()
  df <- sim_data_confounder(seed = 303)
  
  rpt <- dag_assist(d, Y ~ X + Z + C + M, df, "X", "Y")
  
  expect_output(print(rpt), "DAGassist report")
  expect_output(print(rpt), "Validation: VALID")
  expect_output(print(rpt), "Bad controls")
  expect_output(print(rpt), "Minimal controls: \\{Z\\}")
})