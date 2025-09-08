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

#' Compare original vs minimal adjustment specification
#'
#' Fits the user-supplied model and a minimal-adjustment model (via
#' `dagitty::adjustmentSets()`), after validating inputs.
#'
#' @param dag A `dagitty` DAG.
#' @param formula A model formula (e.g., `Y ~ X + Z`).
#' @param data A data.frame with the variables used in `formula`.
#' @param exposure Character; exposure variable name.
#' @param outcome Character; outcome variable name.
#' @param engine A fitting function (default `stats::lm`).
#' @param engine_args List of extra arguments passed to `engine`.
#' @details
#'  will want to edit so it works with fixed/random effects and other 
#' common uses. eventually will want to make sure it works with everything
#' @return An object of class `DAGassist_compare` with elements:
#' `validation`, `original`, `minimal`, `controls`, `formulas`.
#' @examples
#' # compare_specs(test_confounder, Y ~ X + Z, test_df, "X", "Y")
#' @export
compare_specs <- function(dag, formula, data, exposure, outcome,
                          engine = stats::lm, engine_args = list()) {
  v <- validate_spec(dag, formula, data, exposure, outcome)
  if (!v$ok) return(list(validation = v))
  
  # original fit
  fit_args1 <- c(list(formula = formula, data = data), engine_args)
  m_orig <- do.call(engine, fit_args1)
  
  # minimal fit (if needed)
  ctrls <- pick_minimal_controls(dag, exposure, outcome)
  f_min <- update_to_controls(exposure, outcome, ctrls)
  fit_args2 <- c(list(formula = f_min, data = data), engine_args)
  m_min <- do.call(engine, fit_args2)
  
  # quick, dependency-free summary
  out <- list(
    validation = v,
    original   = m_orig,
    minimal    = m_min,
    controls   = ctrls,
    formulas   = list(original = formula, minimal = f_min)
  )
  class(out) <- c("DAGassist_compare", class(out))
  out
}

#' Prints a concise comparison report
#' 
#' @details
#' Prints the output of `compare_specs()`, showing validation status, minimal
#' controls, original/minimal formulas, and a compact coefficient formula.  may
#' want to clean for kable or latex output later. or could maybe wrap in a 
#' capture.output since it uses cat.
#' @param x A `DAGassist_compare` object returned by [compare_specs()].
#' @param ... Ignored.
#'
#' @return Invisibly returns `x`.
#' @examplesIf requireNamespace("dagitty", quietly = TRUE)
#' set.seed(1)
#' d <- dagitty::dagitty("dag { Z -> X; Z -> Y; X -> Y }")
#' n <- 100
#' Z <- rnorm(n); X <- 0.8*Z + rnorm(n); Y <- 1.2*X + 0.5*Z + rnorm(n)
#' df <- data.frame(Y, X, Z)
#' cmp <- compare_specs(d, Y ~ X + Z, df, exposure = "X", outcome = "Y")
#' print(cmp)
#' 
#' @importFrom stats coef
#' @importFrom utils head
print.DAGassist_compare <- function(x, ...) {
  cat("DAGassist compare\n")
  if (!x$validation$ok) {
    cat("Validation: INVALID - see issues below\n")
    print(x$validation); return(invisible(x))
  }
  cat("Validation: VALID\n")
  cat("\nControls (minimal): {", paste(x$controls, collapse = ", "), "}\n", sep = "")
  cat("\nFormulas:\n  original: ", deparse(x$formulas$original), 
      "\n  minimal : ", deparse(x$formulas$minimal), "\n", sep = "")
  cat("\nOriginal fit (coef head):\n"); print(head(coef(summary(x$original))))
  cat("\nMinimal  fit (coef head):\n"); print(head(coef(summary(x$minimal))))
  invisible(x)
}