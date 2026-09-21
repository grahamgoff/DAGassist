library(dagitty)
library(ggdag)
library(DAGassist)

x_pos <- c(
  turnout = 10,
  income = 0,
  state = 5,
  age  = 5,
  polint = 5,
  industry = 0,
  elect_comp = 10
)

y_pos <- c(
  turnout = 0,
  income = 0,
  state = 1,
  age  = -1,
  polint = 0.5,
  industry = -0.5,
  elect_comp = -0.5
)


dag_model <- dagify(
  
  turnout ~ income + state + age + polint + elect_comp,
  
  income ~ state + age + industry,
  
  polint ~ income,
  
  industry ~ age,
  
  exposure = "income",
  outcome  = "turnout",
  
  coords = list(x= x_pos, y = y_pos),
  
  labels = c(
    turnout = "Turnout",
    income = "Income",
    state = "State",
    age  = "Age",
    polint = "Political Interest",
    industry = "Industry",
    elect_comp = "Election Competitiveness"
  )
)

set.seed(42)
n <- 5000

# exogenous
state <- rnorm(n)                                   
age <- rnorm(n)
elect_comp <- rnorm(n)                                  

# structural equations, following the DAG above
industry <- 0.50 * age + rnorm(n)                        
income <- 0.60 * state + 0.50 * age + 0.40 * industry + rnorm(n)
polint <- 0.50 * income + rnorm(n)                   
turnout <- 0.30 * income + 0.40 * polint +
  0.35 * state  + 0.25 * age +
  0.50 * elect_comp + rnorm(n)

df <- data.frame(turnout, income, state, age, polint, industry, elect_comp)

DAGassist(dag = dag_model, 
          formula = lm(turnout ~ income + state + age + polint + industry + elect_comp, data = df),
          estimand = c("total"),
          type = "dotwhisker",
          out = "dev/dotwhisker.png"
)

DAGassist(dag = dag_model, 
          formula = lm(turnout ~ income + state + age + polint + industry + elect_comp, data = df),
          estimand = c("total"),
          type = "latex",
          out = "dev/latex.tex"
)

DAGassist(dag = dag_model, 
          formula = lm(turnout ~ income + state + age + polint + industry + elect_comp, data = df),
          estimand = c("total"),
          type = "excel",
          out = "dev/excel.xlsx"
)

DAGassist(dag = dag_model, 
          formula = lm(turnout ~ income + state + age + polint + industry + elect_comp, data = df),
          estimand = c("total"),
          type = "word",
          out = "dev/word.docx"
)

DAGassist(dag = dag_model, 
          formula = lm(turnout ~ income + state + age + polint + industry + elect_comp, data = df),
          estimand = c("total"),
          type = "text",
          out = "dev/text.txt"
)