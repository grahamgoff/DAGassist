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

#' @rdname test_dags
"test_confounder"

#' @rdname test_dags
"test_collider"

#' @rdname test_dags
"test_mediator"