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

test_cycle <- dagify(
  Y~X,
  Z~Y,
  X~Z,
  exposure = "X",
  outcome = "Y"
)

test_multimin <- dagify(
  Y ~ X + M + W,  
  X ~ Z + W,      
  M ~ Z,          
  exposure = "X",
  outcome  = "Y"
)

test_multimin4 <- ggdag::dagify(
  Y ~ X + M + N,  
  X ~ Z + W,      
  M ~ Z,         
  N ~ W,          
  exposure = "X",
  outcome  = "Y"
)

test_complex <- dagify(
  Y ~ X + M + Z + B,
  M ~ X + A,
  X ~ Z,
  C ~ X + Y,
  exposure = "X",
  outcome = "Y"
)

## Simulate data that matches the relationships
n <- 400

Z <- rnorm(n)       
N <- rnorm(n)  
W <- rnorm(n)               
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

test_df <- data.frame(X, Y, Z, C, M, A, B, N, W)

## add complexity for testing fixed effects and clustering
# Add panel-style groups: id and time (4 periods × 100 ids = 400 obs)
n_id <- 100
T    <- n / n_id   
stopifnot(T == round(T))  # ensure divisibility

test_df$id   <- factor(rep(seq_len(n_id), each = T))
test_df$time <- factor(rep(seq_len(T),     times = n_id))

# add regions for clustering
set.seed(1)
regions <- factor(sample(LETTERS[1:10], n_id, replace = TRUE))
test_df$region <- regions[as.integer(test_df$id)]

# build clustered errors and a parallel outcome Y2 so clustering/FE actually 
# do something substantive.
set.seed(2)
u_id   <- rnorm(n_id,  sd = 0.8)[as.integer(test_df$id)]   # id-level shock
u_time <- rnorm(T,     sd = 0.4)[as.integer(test_df$time)] # time shock
e_ind  <- rnorm(n,     sd = 0.8)                           # idiosyncratic

eps_clustered <- 0.7*u_id + 0.3*u_time + e_ind
test_df$Y2 <- 1.0*test_df$X + 0.6*test_df$Z + 0.6*test_df$M + eps_clustered


usethis::use_data(test_confounder, test_collider, test_mediator, test_complex, 
                  test_df, test_cycle,test_multimin,test_multimin4, overwrite = TRUE)
