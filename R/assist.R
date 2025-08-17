# Run the DAGassist pipeline and print a compact report, tying it all together
#' need to populate this roxy with info later
#' @export
dag_assist <- function(dag, formula, data, exposure, outcome,
                       engine = stats::lm, engine_args = list()) {
  v <- validate_spec(dag, formula, data, exposure, outcome)
  if (!v$ok) return(list(validation = v))
  
  roles <- classify_nodes(dag, exposure, outcome)
  
  # what controls did the user use?
  rhs_terms <- attr(stats::terms(formula), "term.labels")
  user_controls <- setdiff(rhs_terms, c(exposure))
  
  bad <- roles$variable[roles$is_mediator | roles$is_collider | roles$is_descendant_of_outcome]
  bad_in_user <- intersect(user_controls, bad)
  
  minimal <- pick_minimal_controls(dag, exposure, outcome)
  f_min   <- update_to_controls(exposure, outcome, minimal)
  
  # fits
  m_orig <- do.call(engine, c(list(formula = formula, data = data), engine_args))
  m_min  <- do.call(engine, c(list(formula = f_min,  data = data), engine_args))
  
  out <- list(validation = v, roles = roles,
              bad_in_user = bad_in_user,
              controls_minimal = minimal,
              formulas = list(original = formula, minimal = f_min),
              models = list(original = m_orig, minimal = m_min))
  class(out) <- c("DAGassist_report", class(out))
  out
}

#' @export
print.DAGassist_report <- function(x, ...) {
  cat("DAGassist report\n")
  cat("Validation: ", if (x$validation$ok) "VALID" else "INVALID", "\n", sep = "")
  if (!x$validation$ok) { print(x$validation); return(invisible(x)) }
  
  cat("\nRoles:\n")
  print(x$roles)  # your pretty roles table
  
  if (length(x$bad_in_user)) {
    cat("\n⚠ Bad controls in your formula: {", paste(x$bad_in_user, collapse = ", "), "}\n", sep = "")
  } else {
    cat("\nNo bad controls detected in your formula.\n")
  }
  
  cat("\nMinimal controls: {", paste(x$controls_minimal, collapse = ", "), "}\n", sep = "")
  cat("\nFormulas:\n  original: ", deparse(x$formulas$original),
      "\n  minimal : ", deparse(x$formulas$minimal), "\n", sep = "")
  cat("\nOriginal fit (coef head):\n"); print(head(coef(summary(x$models$original))))
  cat("\nMinimal  fit (coef head):\n"); print(head(coef(summary(x$models$minimal))))
  invisible(x)
}