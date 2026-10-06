##builds the datasets shipped in data/.
library(ggdag)
library(dagitty)

set.seed(42)

x_pos <- c(turnout = 10, income = 0, parental_ses = 5, age = 5,
           polint = 5, industry = 0, elect_comp = 10)
y_pos <- c(turnout = 0, income = 0, parental_ses = 1, age = -1,
           polint = 0.5, industry = -0.75, elect_comp = -0.75)
################################### TURNOUT ####################################
##make DAG
turnout_dag <- dagify(
  turnout ~ income + parental_ses + age + polint + elect_comp,
  income ~ parental_ses + age + industry,
  polint ~ income,
  industry ~ age,
  
  exposure = "income",
  outcome  = "turnout",
  
  coords = list(x= x_pos, y = y_pos),
  
  labels = c(
    turnout = "Turnout",
    income = "Income",
    parental_ses = "Parental SES",
    age  = "Age",
    polint = "Political Interest",
    industry = "Industry",
    elect_comp = "Election Competitiveness"
  )
)

##simulate data
n <- 5000

# exogenous
parental_ses <- rnorm(n)                                   
age <- rnorm(n)
elect_comp <- rnorm(n)                                  

# structural equations, following the DAG above
industry <- 0.50 * age + rnorm(n)                        
income <- 0.60 * parental_ses + 0.50 * age + 0.40 * industry + rnorm(n)
polint <- 0.50 * income + rnorm(n)                   
turnout <- 0.30 * income + 0.40 * polint +
  0.35 * parental_ses  + 0.25 * age +
  0.50 * elect_comp + rnorm(n)

turnout_data <- data.frame(turnout, income, parental_ses, age, polint, industry, elect_comp)

#################################### TOY #######################################
##make DAG
toy_dag <- dagify(
  Y ~ X + M + Z + A + B,
  X ~ Z,
  C ~ X + Y,
  M ~ X,
  exposure = "X",
  outcome  = "Y"
)

##simulate data
n <- 2000

# exogenous
A <- rnorm(n, 0, 1)
B <- rnorm(n, 0, 1)
Z <- rnorm(n, 0, 1)

# structural equations
X <- 0.8 * Z + rnorm(n, 0, 1)                                  # X ~ Z
M <- 0.9 * X + rnorm(n, 0, 1)                                  # M ~ X
Y <- 0.7*X + 0.6*M + 0.3*Z + 0.2*A - 0.1*B + rnorm(n, 0, 1)    # Y ~ X + M + Z + A + B
C <- 0.5*X + 0.4*Y + rnorm(n, 0, 1)                            # C ~ X + Y

toy_data <- data.frame(A, B, Z, X, M, Y, C)

##ship
usethis::use_data(
  turnout_dag, turnout_data, toy_dag, toy_data,
  overwrite = TRUE
)