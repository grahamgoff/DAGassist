test_that("roles: confounder / mediator / collider on-path", {
  
  ## confounder
  d1 <- dagitty::dagitty("dag { Z -> X; Z -> Y; X -> Y }")
  r1 <- classify_nodes(d1, "X","Y")
  expect_equal(r1$role[r1$variable=="Z"], "confounder")
  
  ## mediator
  d2 <- dagitty::dagitty("dag { X -> M -> Y }")
  r2 <- classify_nodes(d2, "X","Y")
  expect_equal(r2$role[r2$variable=="M"], "mediator")
  
  ## collider
  d3 <- dagitty::dagitty("dag { X -> C <- Z -> Y }")   
  r3 <- classify_nodes(d3, "X","Y")
  expect_true(r3$is_collider[r3$variable=="C"])
  
  ## no collider
  d4 <- dagitty::dagitty("dag { X -> Y; A -> C <- B }") 
  r4 <- classify_nodes(d4, "X","Y")
  expect_false(r4$is_collider[r4$variable=="C"])
})