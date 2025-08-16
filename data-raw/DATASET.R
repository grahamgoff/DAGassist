## code to prepare `DATASET` dataset goes here
library(ggdag)

test_confounder <- dagify(
  Y ~ X + Z,
  X ~ Z,
  exposure = "X",
  outcome = "Y"
)

test_collider <- dagify(
  Y ~ X,
  C ~ X + Y,
  exposure = "X",
  outcome = "Y"
)

test_mediator <- dagify(
  Y ~ M,
  M ~ X,
  exposure = "X",
  outcome = "Y"
)

X <- rnorm(30)
Y <- rnorm(30)
Z <- rnorm(30)
C <- rnorm(30)
M <- rnorm(30)
test_df <- data.frame(X, Y, Z, C, M) 
partial_test_df <- data.frame(X, Y, Z)

usethis::use_data(test_confounder, test_collider, test_mediator, test_df,
                  partial_test_df, overwrite = TRUE)
