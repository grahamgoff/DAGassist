#' Toy data for demonstrating causal roles
#'
#' A small simulated dataset in which each causal role that [DAGassist()]
#' classifies appears at least once. Generated from [toy_dag], so the
#' correct adjustment set is known by construction: the total effect of
#' `X` on `Y` requires adjusting for `Z` alone.
#'
#' The data-generating process is linear and Gaussian with
#' `set.seed(42)`. True coefficients are `X -> Y = 0.7`, `M -> Y = 0.6`,
#' `Z -> Y = 0.3`, `A -> Y = 0.2`, `B -> Y = -0.1`.
#'
#' @format A data frame with 2,000 rows and 7 columns:
#' \describe{
#'   \item{X}{Exposure. Caused by `Z`.}
#'   \item{Y}{Outcome. Caused by `X`, `M`, `Z`, `A`, and `B`.}
#'   \item{Z}{Confounder: a common cause of `X` and `Y`.}
#'   \item{M}{Mediator: lies on the path `X -> M -> Y`.}
#'   \item{C}{Collider: a common descendant of `X` and `Y`.}
#'   \item{A}{Neutral control on the outcome; affects `Y` only.}
#'   \item{B}{Neutral control on the outcome; affects `Y` only.}
#' }
#' @source Simulated by `data-raw/make_data.R`.
#' @seealso [toy_dag] for the generating graph.
"toy_data"

#' DAG behind [toy_data]
#'
#' The graph used to simulate [toy_data], with `X` marked as the exposure
#' and `Y` as the outcome. Pass it directly to [DAGassist()].
#'
#' @format A `dagitty` object with 7 nodes.
#' @source Defined in `data-raw/make_data.R`.
#' @seealso [toy_data]
"toy_dag"

#' Simulated data on income and voter turnout
#'
#' A simulated cross-section used to demonstrate the [DAGassist()] workflow on
#' an applied question: does higher income increase voter turnout? Because the
#' data-generating process is known, the correct answer is known too, and the
#' cost of a misspecified control set can be read directly off the output.
#'
#' Generated from [turnout_dag] with `set.seed(42)`. Income raises turnout
#' directly (`0.30`) and indirectly by raising political interest
#' (`0.50 * 0.40 = 0.20`), so the **true total effect is `0.50`** and the
#' **true direct effect is `0.30`**.
#'
#' Adjusting for `{age, state}` recovers the total effect; adjusting for
#' `{age, polint, state}` recovers the direct effect. Regressing `turnout` on
#' every available covariate returns roughly `0.28` — close to the direct
#' effect, because conditioning on the mediator `polint` silently changes the
#' estimand rather than producing an obviously wrong number.
#'
#' @format A data frame with 5,000 rows and 7 columns:
#' \describe{
#'   \item{turnout}{Outcome. Voter turnout propensity.}
#'   \item{income}{Exposure. Caused by `state`, `age`, and `industry`.}
#'   \item{state}{Confounder: a common cause of `income` and `turnout`.}
#'   \item{age}{Confounder: affects `income`, `turnout`, and `industry`.}
#'   \item{polint}{Political interest. A mediator on the path
#'     `income -> polint -> turnout`. Adjusting for it removes the indirect
#'     effect and is a bad control for the total effect.}
#'   \item{industry}{A cause of `income` lying on the back-door path
#'     `income <- industry <- age -> turnout`. Safe to adjust for, but not
#'     required once `age` is included.}
#'   \item{elect_comp}{Election competitiveness. A neutral control on the
#'     outcome: it affects `turnout` only, so adjusting for it leaves the
#'     estimate unbiased and improves precision.}
#' }
#' @source Simulated by `data-raw/make_data.R`.
#' @seealso [turnout_dag] for the generating graph; [toy_data] for a smaller
#'   example covering every causal role.
"turnout_data"

#' DAG behind [turnout_data]
#'
#' The graph used to simulate [turnout_data], with `income` marked as the
#' exposure and `turnout` as the outcome. Node coordinates and display labels
#' are attached, so it plots directly with `ggdag::ggdag_status()`. Pass it to
#' [DAGassist()] to classify covariates and re-estimate against DAG-derived
#' adjustment sets.
#'
#' Minimal sufficient adjustment sets, as returned by
#' `dagitty::adjustmentSets()`:
#' \itemize{
#'   \item Total effect: `{age, state}`
#'   \item Direct effect: `{age, polint, state}`
#'   \item Canonical (total): `{age, elect_comp, industry, state}`
#' }
#'
#' @format A `dagitty` object with 7 nodes and 10 edges.
#' @source Defined in `data-raw/make_data.R`.
#' @seealso [turnout_data]
"turnout_dag"