test_that("uncertain edge flips the canonical set and recommends re-estimation", {
  skip_if_not_installed("dagitty")
  g <- dagitty::dagitty("dag { Z->X; X->Y; Z->Y; A->B; B->Y }")
  dagitty::exposures(g) <- "X"; dagitty::outcomes(g) <- "Y"
  s <- pdag_robustness(g, uncertain_edges = "A -- B")
  expect_s3_class(s, "DAGassist_pdag_summary")
  expect_equal(s$n_worlds, 2L)
  expect_true(s$canonical_changed)
})

test_that("cyclic orientations are dropped", {
  skip_if_not_installed("dagitty")
  g <- dagitty::dagitty("dag { A->B; B->C; C->A; A->Y; B->Y }")  # already-directed triangle
  dagitty::exposures(g) <- "A"; dagitty::outcomes(g) <- "Y"
  s <- pdag_robustness(g, uncertain_edges = c("A -- B", "B -- C"))
  expect_true(s$n_worlds < 4L)            # at least one orientation is cyclic
})

test_that("no spurious change when uncertainty is irrelevant to X->Y", {
  skip_if_not_installed("dagitty")
  g <- dagitty::dagitty("dag { Z->X; X->Y; Z->Y; P->Q }")  # P--Q off the X-Y system
  dagitty::exposures(g) <- "X"; dagitty::outcomes(g) <- "Y"
  s <- pdag_robustness(g, uncertain_edges = "P -- Q")
  expect_false(s$minimal_changed)
  expect_false(s$reestimate)
})

test_that("DAGassist() attaches a pdag summary", {
  skip_if_not_installed("dagitty")
  g <- dagitty::dagitty("dag { Z->X; X->Y; Z->Y; A->B; B->Y }")
  dagitty::exposures(g) <- "X"; dagitty::outcomes(g) <- "Y"
  df <- data.frame(Z=rnorm(50), X=rnorm(50), Y=rnorm(50), A=rnorm(50), B=rnorm(50))
  r <- DAGassist(g, lm(Y ~ X + Z, data = df), uncertain_edges = "A -- B")
  expect_s3_class(r$pdag, "DAGassist_pdag_summary")
})