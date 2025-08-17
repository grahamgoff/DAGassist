## code to prepare `DATASET` dataset goes here
library(ggdag)

## realistic DAGs
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

test_complex <- dagify(
  Y ~ X + M + Z,
  M ~ X + A,
  X ~ Z,
  C ~ X + Y,
  exposure = "X",
  outcome = "Y"
)

## Simulate data that matches the relationships
n <- 400

Z <- rnorm(n)               
A <- rnorm(n) # helps with calculating M              
B <- rnorm(n) # random noise

# Structural equations (linear + noise); tune coefficients for signal
eX <- rnorm(n, sd = 0.6)
X  <- 0.9*Z + eX

eM <- rnorm(n, sd = 0.6)
M  <- 0.7*X + 0.5*A + eM

eY <- rnorm(n, sd = 0.8)
Y  <- 1.0*X + 0.6*Z + 0.6*M + eY

eC <- rnorm(n, sd = 0.6)
C  <- 0.8*X + 0.8*Y + eC 

test_df <- data.frame(X, Y, Z, C, M, A, B)

usethis::use_data(test_confounder, test_collider, test_mediator, test_complex, 
                  test_df, overwrite = TRUE)
