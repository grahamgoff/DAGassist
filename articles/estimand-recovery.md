# Recovering Estimands with DAGassist

``` r
library(DAGassist)
```

From Denly email:

The problem is that, for observational regressions with controls, linear
regression and generalized linear model methods (e.g., logit) do not
recover average treatment effects by default. Instead, they recover
weighted strata-specific averages. Take a peak at the Chattopadhyay and
Zubizarreta’s (2024) Harvard Data Science Review article in Mendeley,
which breaks things down in a very friendly way

## Example: The Causal Effects of Family Background and Life Course Events on Fertility Patterns

Attribute to Morgan and Winship (p. 17-19)

| variable      | type    | Min     | Q1       | Median   | Mean     | Q3        | Max       |
|:--------------|:--------|:--------|:---------|:---------|:---------|:----------|:----------|
| id            | integer | 1.00    | 250.75   | 500.50   | 500.50   | 750.25    | 1000.00   |
| year          | integer | 0.00    | 1.00     | 2.00     | 2.00     | 3.00      | 4.00      |
| age           | numeric | 0.00    | 27.60    | 37.70    | 37.76    | 47.40     | 86.20     |
| edu_year      | numeric | 0.00    | 11.80    | 13.10    | 13.07    | 15.20     | 22.00     |
| married       | integer | 0.00    | 0.00     | 1.00     | 0.54     | 1.00      | 1.00      |
| birth_control | integer | 0.00    | 0.00     | 1.00     | 0.69     | 1.00      | 1.00      |
| income        | numeric | 2277.00 | 33552.50 | 62842.00 | 80974.87 | 107020.75 | 784812.00 |
| children      | numeric | 0.00    | 0.00     | 1.00     | 1.96     | 2.00      | 12.00     |

| variable   | type    | top_levels                                              |
|:-----------|:--------|:--------------------------------------------------------|
| gender     | factor  | Male:2565 Female:2435                                   |
| immigrant  | factor  | No:4380 Yes:620                                         |
| urban      | factor  | Urban:3560 Rural:1440                                   |
| class      | ordered | Working:2080 Middle:1580 Low:885 (Other):455            |
| religion   | factor  | Christian:1965 Unaffiliated:1665 Muslim:450 (Other):920 |
| edu_degree | factor  | HS_grad:1635 Some_college:1390 BA:950 (Other):1025      |

    #> [1] TRUE
    #> [1] TRUE
    #> list()

![](estimand-recovery_files/figure-html/example-dag-1.png)

Boilerplate `DAGassist`:

``` r
DAGassist(dag_model,
          formula = lm(children ~ edu_year + age + class + gender + 
                         immigrant + urban + birth_control + income + 
                         married + religion, data = dat),
          type="txt")
```

## DAGassist Report:

## Roles

| Variable      |    Role    | Exp. | Out. | `CON` | `MED` | `COL` | `dOut` | `dMed` | `dCol` | dConfOn | dConfOff | `NCT` | `NCO` |
|:--------------|:----------:|:----:|:----:|:-----:|:-----:|:-----:|:------:|:------:|:------:|:-------:|:--------:|:-----:|:-----:|
| age           | confounder |      |      |   x   |       |       |        |        |        |         |          |       |       |
| birth_control |  mediator  |      |      |       |   x   |       |        |   x    |        |         |          |       |       |
| children      |  outcome   |      |  x   |       |       |       |        |        |        |         |          |       |       |
| class         | confounder |      |      |   x   |       |       |        |        |        |         |          |       |       |
| edu_year      |  exposure  |  x   |      |       |       |       |        |        |        |         |          |       |       |
| gender        | confounder |      |      |   x   |       |       |        |        |        |         |          |       |       |
| immigrant     | confounder |      |      |   x   |       |       |        |        |        |         |          |       |       |
| income        |  mediator  |      |      |       |   x   |       |        |        |        |         |          |       |       |
| married       |  mediator  |      |      |       |   x   |       |        |   x    |        |         |          |       |       |
| religion      |  mediator  |      |      |       |   x   |       |        |        |        |         |          |       |       |
| urban         | confounder |      |      |   x   |       |       |        |        |        |         |          |       |       |

### Models

| Term                 |   Original   |  Minimal 1   |  Canonical   |
|:---------------------|:------------:|:------------:|:------------:|
| edu_year             | -0.047\*\*\* | -0.054\*\*\* | -0.054\*\*\* |
|                      |   (0.013)    |   (0.013)    |   (0.013)    |
| age                  | 0.066\*\*\*  | 0.079\*\*\*  | 0.079\*\*\*  |
|                      |   (0.004)    |   (0.003)    |   (0.003)    |
| genderMale           |    -0.046    |    -0.037    |    -0.037    |
|                      |   (0.083)    |   (0.082)    |   (0.082)    |
| immigrantYes         |    0.121     |    0.098     |    0.098     |
|                      |   (0.124)    |   (0.124)    |   (0.124)    |
| urbanUrban           |    -0.128    |    -0.136    |    -0.136    |
|                      |   (0.091)    |   (0.091)    |   (0.091)    |
| birth_control        |    -0.103    |              |              |
|                      |   (0.095)    |              |              |
| income               |   0.000\*    |              |              |
|                      |   (0.000)    |              |              |
| married              | 0.456\*\*\*  |              |              |
|                      |   (0.117)    |              |              |
| religionMuslim       |    0.059     |              |              |
|                      |   (0.151)    |              |              |
| religionHindu        |  -0.611\*\*  |              |              |
|                      |   (0.222)    |              |              |
| religionBuddhist     |    0.230     |              |              |
|                      |   (0.222)    |              |              |
| religionJewish       |    0.130     |              |              |
|                      |   (0.215)    |              |              |
| religionUnaffiliated | -0.468\*\*\* |              |              |
|                      |   (0.096)    |              |              |
| religionOther        |   -0.350\*   |              |              |
|                      |   (0.167)    |              |              |
| Num.Obs.             |     5000     |     5000     |     5000     |
| R2                   |    0.153     |    0.142     |    0.142     |

#### Notes

- Roles legend: X (exposure); Y (outcome); CON (confounder); MED
  (mediator); COL (collider); dOut (proper descendant of Y); dMed
  (proper descendant of any mediator); dCol (proper descendant of any
  collider); dConfOn (descendant of a confounder on a back-door path);
  dConfOff (descendant of a confounder off a back-door path); NCT
  (neutral control on treatment); NCO (neutral control on outcome).
- p-value legend: + \< 0.1, \* \< 0.05, \*\* \< 0.01, \*\*\* \< 0.001.
- Controls (minimal): {age, class, gender, immigrant, urban}.
- Controls (canonical): {age, class, gender, immigrant, urban}.
