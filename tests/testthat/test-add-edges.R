test_that("directed edge that creates a confounder changes the minimal set", {
  skip_if_not_installed("dagitty")
  g <- dagitty::dagitty("dag { Z->X; X->Y }"); dagitty::exposures(g)<-"X"; dagitty::outcomes(g)<-"Y"
  s <- add_edges_robustness(g, add_edges = "Z -> Y")
  expect_s3_class(s, "DAGassist_addedge_summary")
  expect_true(s$per_edge$minimal_changed[1]); expect_true(s$reestimate)
})
test_that("bidirected X<->Y breaks identification", {
  skip_if_not_installed("dagitty")
  g <- dagitty::dagitty("dag { Z->X; X->Y }"); dagitty::exposures(g)<-"X"; dagitty::outcomes(g)<-"Y"
  expect_false(add_edges_robustness(g, add_edges = "X <-> Y")$per_edge$identifiable[1])
})
test_that("cycle-inducing edge is flagged, not evaluated", {
  skip_if_not_installed("dagitty")
  g <- dagitty::dagitty("dag { Z->X; X->Y }"); dagitty::exposures(g)<-"X"; dagitty::outcomes(g)<-"Y"
  expect_match(add_edges_robustness(g, add_edges = "Y -> Z")$per_edge$note[1], "cycle")
})
test_that("engine call as formula does not fit", {
  skip_if_not_installed("dagitty")
  g <- dagitty::dagitty("dag { Z->X; X->Y }"); dagitty::exposures(g)<-"X"; dagitty::outcomes(g)<-"Y"
  d <- data.frame(X=rnorm(40), Y=rnorm(40), Z=rnorm(40))
  expect_s3_class(add_edges_robustness(g, add_edges="Z -> Y", formula=lm(Y~X, data=d)),
                  "DAGassist_addedge_summary")
})