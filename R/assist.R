######################## NON-EXPORTED HELPER FUNCTIONS #########################
## infer x and y from the call, so the user does not have to make an extra 
## "engine" call 
.infer_xy <- function(dag, exposure, outcome) {
  # If either exposure or outcome is missing/empty, infer from dagitty
  if (missing(exposure) || is.null(exposure) || !nzchar(exposure)) {
    ex <- tryCatch(dagitty::exposures(dag), error = function(e) character(0))
    if (length(ex) == 1) exposure <- ex else {
      stop("Please supply `exposure=`; DAG has ", length(ex), " exposure(s).", call. = FALSE)
    }
  }
  if (missing(outcome) || is.null(outcome) || !nzchar(outcome)) {
    out <- tryCatch(dagitty::outcomes(dag), error = function(e) character(0))
    if (length(out) == 1) outcome <- out else {
      stop("Please supply `outcome=`; DAG has ", length(out), " outcome(s).", call. = FALSE)
    }
  }
  list(exposure = exposure, outcome = outcome)
}

# Is an expression a ~-formula?
.is_formula_expr <- function(expr) {
  is.call(expr) && identical(expr[[1]], as.name("~"))
}

## Extract engine, formula, data, and extra args from an unevaluated call like
##   feols(Y ~ X + Z | A, data = df, cluster = ~id)
## to avoid separate, explicit "formula", "data", "engine", and "engine_args"
## arguments. 
.extract_from_engine_call <- function(expr, eval_env = parent.frame()) {
  stopifnot(is.call(expr))
  fn_name <- as.character(expr[[1]])
  # find the function; respects namespaces if user wrote fixest::feols
  fn <- eval(expr[[1]], envir = eval_env)
  
  # turn call into a named list of arguments (some may be unnamed/positional)
  args_list <- as.list(expr)[-1]
  arg_names <- names(args_list)
  if (is.null(arg_names)) arg_names <- rep("", length(args_list))
  
  # locate the formula: prefer 'fml' or 'formula', else first positional
  f_idx <- which(arg_names %in% c("fml", "formula"))[1]
  if (is.na(f_idx)) f_idx <- 1L
  f_expr <- args_list[[f_idx]]
  fml   <- eval(f_expr, envir = eval_env)
  
  # locate data: prefer 'data', else next positional after fml
  d_idx <- which(arg_names == "data")[1]
  if (is.na(d_idx)) {
    # next positional after f_idx
    pos <- seq_along(args_list)
    pos <- pos[arg_names == ""]
    pos <- pos[pos > f_idx]
    if (length(pos)) d_idx <- pos[1] else d_idx <- NA_integer_
  }
  if (is.na(d_idx)) {
    stop("Could not find `data=` in the engine call. Please supply data.", call. = FALSE)
  }
  data_expr <- args_list[[d_idx]]
  data_obj  <- eval(data_expr, envir = eval_env)
  
  # everything else becomes engine_args
  keep <- setdiff(seq_along(args_list), c(f_idx, d_idx))
  extra <- args_list[keep]
  # evaluate each remaining arg now so do.call can use them
  if (length(extra)) {
    engine_args <- lapply(extra, function(e) eval(e, envir = eval_env))
    names(engine_args) <- names(extra)
  } else {
    engine_args <- list()
  }
  
  list(engine = fn, formula = fml, data = data_obj, engine_args = engine_args)
}

# Strip fixest FE/IV parts for parsing RHS terms, and return:
# - base formula (pre-`|`)
# - tail string (the " | FE | IV ..." to re-attach later), or "" if none
.strip_fixest_parts <- function(fml) {
  s <- paste(deparse(fml, width.cutoff = 500), collapse = " ")
  parts <- strsplit(s, "\\|")[[1]]
  base <- trimws(parts[1])
  tail <- if (length(parts) >= 2) paste0(" | ", paste(trimws(parts[-1]), collapse = " | ")) else ""
  base_fml <- stats::as.formula(base, env = environment(fml))
  list(base = base_fml, tail = tail)
}

# Build a minimal formula that preserves any fixest '|' tail if present
.build_minimal_formula <- function(orig_fml, exposure, outcome, minimal_controls) {
  sp <- .strip_fixest_parts(orig_fml)
  # main RHS we control
  rhs <- paste(c(exposure, minimal_controls), collapse = " + ")
  # if no controls, ensure just exposure appears on RHS
  rhs <- if (nzchar(rhs)) rhs else exposure
  if (nzchar(sp$tail)) {
    # Recompose string then parse
    f_str <- paste0(outcome, " ~ ", rhs, sp$tail)
    stats::as.formula(f_str, env = environment(orig_fml))
  } else {
    # simple case
    update_to_controls(exposure, outcome, minimal_controls)
  }
}

# Get RHS term labels from the pre-| part (so fixest doesn't confuse terms())
.rhs_terms_safe <- function(fml) {
  base <- .strip_fixest_parts(fml)$base
  attr(stats::terms(base), "term.labels")
}


################################################################################
#' Main function
#' Run the DAGassist pipeline and print a compact report, tying it all together
#' 
#' @param dag a valid dagitty object
#' @param formula a regression formula *or* a single engine call--e.g.
#' `feols(Y~X+Z | A, data = df, cluster = ~id)` 
#' @param data a valid data frame, (optional if supplied via formula call)
#' @param exposure optional; inferred from the DAG if missing
#' @param outcome optional; inferred from DAG if missing 
#' @param engine the regression engine; default `stats::lm`
#' @param engine_args a list with exra engine args---merged with any found in call
#' 
#' @return a list of class `out` with the `DAGassist_report`, which has values
#'         validation: runs `validate_spec` from validate.R
#'         roles: runs `classify_nodes` from classify.R
#'         bad_in_user: does the user include mediator, collider or descendant as controls?
#'         controls_minimal: runs `pick_minimal_controls` from compare.R
#'         formulas: a list with the original regression formula + minimal formula
#'         models: a list with the full original regression call + minimal regression call
#'         
#' @examplesIf requireNamespace("dagitty", quietly = TRUE)
#' # Use package datasets to avoid re-simulating
#' data(test_df, package = "DAGassist")
#' data(test_complex, package = "DAGassist")
#' dag_assist(test_complex, Y ~ X + Z + C + M, test_df, exposure = "X", outcome = "Y")
#' 
#' @export

dag_assist <- function(dag, formula, data, exposure, outcome,
                       engine = stats::lm, engine_args = list()) {
  
  # 1) Allow formula to be either a formula or a single engine call
  spec_expr <- substitute(formula)  # capture unevaluated argument
  parsed <- NULL
  
  if (is.call(spec_expr) && !identical(spec_expr[[1]], as.name("~"))) {
    # User passed an engine call like feols(y ~ x | fe, data = df, ...)
    parsed <- .extract_from_engine_call(spec_expr, eval_env = parent.frame())
    engine      <- parsed$engine
    formula     <- parsed$formula
    # only fill data if user didn't also pass `data=` explicitly
    if (missing(data) || is.null(data)) data <- parsed$data
    # merge engine_args: call args take precedence; user-supplied list can add/override
    engine_args <- utils::modifyList(parsed$engine_args, engine_args)
  } else {
    # User passed a plain formula; keep engine and data as provided
    # nothing to do here
  }
  
  # 2) Infer exposure/outcome from DAG if user didn't set them
  xy <- .infer_xy(dag, exposure, outcome)
  exposure <- xy$exposure
  outcome  <- xy$outcome
  
  # Validate inputs using the now-normalized pieces
  v <- validate_spec(dag, formula, data, exposure, outcome)
  if (!v$ok) return(list(validation = v))
  
  # Classify nodes
  roles <- classify_nodes(dag, exposure, outcome)
  
  # what controls did the user use? (only from the pre-| part if present)
  rhs_terms <- .rhs_terms_safe(formula)
  user_controls <- setdiff(rhs_terms, c(exposure))
  
  # "bad controls"
  bad <- roles$variable[roles$is_mediator | roles$is_collider | roles$is_descendant_of_outcome]
  bad_in_user <- intersect(user_controls, bad)
  
  # minimal set
  minimal <- pick_minimal_controls(dag, exposure, outcome)
  f_min   <- .build_minimal_formula(formula, exposure, outcome, minimal)
  
  # fits: pass formula & data positionally (engine-agnostic: lm, feols, lmer, etc.)
  m_orig <- do.call(engine, c(list(formula, data), engine_args))
  m_min  <- do.call(engine, c(list(f_min,  data), engine_args))
  
  out <- list(validation = v, 
              roles = roles,
              bad_in_user = bad_in_user,
              controls_minimal = minimal,
              formulas = list(original = formula, minimal = f_min),
              models = list(original = m_orig, minimal = m_min))
  class(out) <- c("DAGassist_report", class(out))
  out
}

#' print the DAGassist_report
#' @param x Output of dag_assist() (class "out")
#' @param ... (ignored)
#' @return Invisibly returns x
#' 
#' @examplesIf requireNamespace("dagitty", quietly = TRUE)
#' # Use package datasets to avoid re-simulating
#' data(test_df, package = "DAGassist")
#' data(test_complex, package = "DAGassist")
#' dag_assist(test_complex, Y ~ X + Z + C + M, test_df, exposure = "X", outcome = "Y")
#' 
#' @export
#' 
print.DAGassist_report <- function(x, ...) {
  cat("DAGassist Report:\n")
  #cat("Validation: ", if (x$validation$ok) "VALID" else "INVALID", "\n", sep = "")
  if (!x$validation$ok) { print(x$validation); return(invisible(x)) }
  
  cat("\nRoles:\n")
  print(x$roles)  # your pretty roles table
  
  if (length(x$bad_in_user)) {
    cat("\n (!) Bad controls in your formula: {", paste(x$bad_in_user, collapse = ", "), "}\n", sep = "")
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