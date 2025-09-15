#' flag bad controls (mediator/collider/desc of Y) among a candidate set
#' 
#' @param dag A `dagitty` DAG object.
#' @param controls Character vector of variable names.
#' @param exposure Character; exposure node name (X).
#' @param outcome  Character; outcome node name (Y).
#'
#' @return A character vector (possibly empty) containing the elements of
#'   `controls` that are identified as "bad controls".
#'   
#' This is essentially the inverse of `pick_minimal_controls()`, as it returns
#' bad controls, rather than the minimal/canonical set of good controls
#' 
#' @examplesIf requireNamespace("dagitty", quietly = TRUE)
#' d <- ggdag::dagify(
#' Y ~ X + M + Z,
#' M ~ X + Z,
#' C ~ X + Y,
#' exposure = "X",
#' outcome = "Y")
#' # M: mediator / Z: confounder / C: collider
#'
#' # hypothetical candidate controls
#' controls <- c("Z", "M", "C")
#'
#' # Flag controls that would bias the total effect of X on Y:
#' bad_controls_in(d, controls = c("Z","M","C"), exposure = "X", outcome = "Y")
#'
#' # expected: c("M", "C")  # mediator & collider are "bad controls"; Z is OK

#' @export
bad_controls_in <- function(dag, controls, exposure, outcome) {
  roles <- classify_nodes(dag, exposure = exposure, outcome = outcome)
  bad  <- roles$variable[roles$is_mediator | roles$is_collider | roles$is_descendant_of_outcome]
  intersect(controls, bad)
}

#' compute minimal adjustment sets and pick one deterministically
#' @param dag A `dagitty` DAG object.
#' @param exposure Character; exposure node name (X).
#' @param outcome  Character; outcome node name (Y).
#' 
#' @return sorted character vector with the minimal adjustment set
#' @details
#' This is preferable to `dagitty::adjustmentSets()` because it picks a single
#' minimal adjustment set automatically and without a bunch of arguments. will 
#' need to adapt this to deal with multiple adjustment sets and canonical 
#' adjustment sets eventually.
#' 
#' @examplesIf requireNamespace("dagitty", quietly = TRUE)
#' d <- ggdag::dagify(
#' Y ~ X + M + Z,
#' M ~ X + Z,
#' C ~ X + Y,
#' exposure = "X",
#' outcome = "Y")
#' # M: mediator / Z: confounder / C: collider
#'
#' # identify the minimal adjustment set
#' pick_minimal_controls(d, exposure = "X", outcome = "Y")
#' # expected: "Z"
#' @noRd
pick_minimal_controls <- function(dag, exposure, outcome) {
  sets <- dagitty::adjustmentSets(dag, exposure = exposure, outcome = outcome)
  # no sets at all: no adjustment needed
  if (length(sets) == 0L) return(character(0))
  
  # cardinality of each set
  sizes <- vapply(sets, length, integer(1))
  
  # candidates with smallest size
  cand <- which(sizes == min(sizes))
  
  # among candidates, pick smallest candidate
  norm_key <- function(s) paste(sort(as.character(unname(s))), collapse = ",")
  keys <- vapply(sets[cand], norm_key, character(1))
  
  idx <- cand[order(keys)][1L]
  
  # return sorted character vector (stable)
  sort(as.character(unname(sets[[idx]])))
}

#' update formula
#' Update a formula to use chosen controls
#' NOTE: this will be used later when we compute minimal and canonical adjustment sets
#' 
#' @param exposure Character; exposure variable name.
#' @param outcome  Character; outcome variable name.
#' @param controls Character vector of controls (default empty).
#' 
#' @importFrom stats reformulate
#' @noRd
update_to_controls <- function(exposure, outcome, controls = character(0)) {
  reformulate(termlabels = c(exposure, controls), response = outcome)
}

