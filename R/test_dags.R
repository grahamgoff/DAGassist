#' Example DAGs for demos and tests
#'
#' Three tiny DAGs: a confounder case, a collider, and a mediator.
#' They are stored as `dagitty` DAG objects.
#'
#' @format Each object is a `dagitty` DAG.
#' @details
#' - `test_confounder`: U -> X, U -> Y, X -> Y
#' - `test_collider`:  X -> C <- Y
#' - `test_mediator`:  X -> M -> Y
#'
#' @examples
#' data(test_confounder, package = "DAGassist")
#' test_confounder
#'
#' @name test_dags
NULL   # this anchors the shared help page

#' Demo DAG with a confounder
#' 
#' @format A `dagitty` object with nodes: X,Y,Z
#' @source Simulated for examples in the package.
"test_confounder"

#' Demo DAG with a collider
#' 
#' @format A `dagitty` object with nodes: X,Y,C
#' @source Simulated for examples in the package.
"test_collider"

#' Demo DAG with a confounder
#' 
#' @format A `dagitty` object with nodes: X,Y,M
#' @source Simulated for examples in the package.
"test_mediator"

#' Demo DAG with confounder, mediator, collider
#'
#' @format A `dagitty` object with nodes: X,Y,Z,M,C,A
#' @source Simulated for examples in the package.
"test_complex"

#' Demo data frame consistent with demo DAG
#'
#' @format A data frame with 400 rows and 6 variables: `X`, `Y`, `Z`, `C`, `M`, `A`.
#' @source Simulated for examples in the package.
"test_df"

#' Partial demo data (missing some variables)
#'
#' @format A data frame with 3 variables: `X`, `Y`, `Z`.
#' @source Simulated for examples in the package.
"partial_test_df"