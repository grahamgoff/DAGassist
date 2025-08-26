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

## Pick a deterministic canonical adjustment set
# hoose the set with the fewest controls and tie-break alpahbetically for stability
# will need to add multiple-set functionality eventually
.pick_canonical_controls <- function(dag, exposure, outcome) {
  sets <- tryCatch(
    dagitty::adjustmentSets(dag, exposure = exposure, outcome = outcome, type = "canonical"),
    error = function(e) NULL
  )
  if (is.null(sets) || length(sets) == 0) return(character(0))
  
  # Normalize to sorted character vectors and defensively drop X/Y if present
  sets <- lapply(sets, function(s) sort(setdiff(as.character(s), c(exposure, outcome))))
  
  lens <- vapply(sets, length, integer(1))
  candidates <- sets[lens == min(lens)]
  keys <- vapply(candidates, function(s) paste(s, collapse = "|"), character(1))
  candidates[[order(keys)[1]]]
}

# Build a formula from control set, preserving any fixest | tail, as in minimal
.build_formula_with_controls <- function(orig_fml, exposure, outcome, controls) {
  sp  <- .strip_fixest_parts(orig_fml)
  rhs <- paste(c(exposure, controls), collapse = " + ")
  rhs <- if (nzchar(rhs)) rhs else exposure
  if (nzchar(sp$tail)) {
    f_str <- paste0(outcome, " ~ ", rhs, sp$tail)
    stats::as.formula(f_str, env = environment(orig_fml))
  } else {
    update_to_controls(exposure, outcome, controls)
  }
}

# Get RHS term labels from the pre-| part (so fixest doesn't confuse terms())
.rhs_terms_safe <- function(fml) {
  base <- .strip_fixest_parts(fml)$base
  attr(stats::terms(base), "term.labels")
}

# Return ALL minimal adjustment sets as a list 
.minimal_sets_all <- function(dag, exposure, outcome) {
  sets <- tryCatch(
    dagitty::adjustmentSets(dag, exposure = exposure, outcome = outcome, type = "minimal"),
    error = function(e) NULL
  )
  if (is.null(sets) || length(sets) == 0) return(list())
  lapply(sets, function(s) sort(setdiff(as.character(s), c(exposure, outcome))))
}

# Pretty formatter for multiple adjustment sets: c("A","B") -> "{A, B}"
.format_set <- function(s) paste0("{", paste(s, collapse = ", "), "}")

# Are two formulas text-identical?
.same_formula <- function(f1, f2) {
  paste(deparse(f1), collapse = " ") == paste(deparse(f2), collapse = " ")
}

# Safe fit wrapper so bad specs (like  FE collinearity) don't crash printing
.safe_fit <- function(engine, fml, data, engine_args) {
  tryCatch(
    do.call(engine, c(list(fml, data), engine_args)),
    error = function(e) structure(
      list(error = conditionMessage(e), formula = fml),
      class = "DAGassist_fit_error"
    )
  )
}

# Flexible comparison printer for a named list of models.
.print_model_comparison_list <- function(mods) {
  if (requireNamespace("modelsummary", quietly = TRUE)) {
    tab <- modelsummary::msummary(
      mods,
      stars = TRUE,
      output   = "markdown",
      gof_omit = "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma"
    )
    cat("\nModel comparison:\n")
    if (is.character(tab)) cat(paste(tab, collapse = "\n")) else print(tab)
    return(invisible(NULL))
  }
  if (requireNamespace("broom", quietly = TRUE)) {
    cat("\nModel comparison (fallback; install {modelsummary} for a nicer table):\n")
    for (nm in names(mods)) {
      m <- mods[[nm]]
      if (inherits(m, "DAGassist_fit_error")) {
        cat("\n", nm, " (fit error): ", m$error, "\n", sep = ""); next
      }
      tt <- tryCatch(broom::tidy(m), error = function(e) NULL)
      if (is.null(tt)) { cat("\n", nm, ": could not be tidied.\n", sep = ""); next }
      cols <- intersect(c("term","estimate","std.error","statistic","p.value"), names(tt))
      cat("\n", nm, ":\n", sep = ""); print(utils::head(tt[, cols, drop = FALSE], 10))
    }
    return(invisible(NULL))
  }
  cat("\nModel comparison (basic):\n")
  for (nm in names(mods)) {
    m <- mods[[nm]]
    cat("\n", nm, " (coef head):\n", sep = "")
    if (inherits(m, "DAGassist_fit_error")) { cat("fit error: ", m$error, "\n", sep = ""); next }
    print(utils::head(tryCatch(stats::coef(m), error = function(e) NULL)))
  }
  invisible(NULL)
}

## helps find the exposure and outcome names for the note on DAG-derived additions
get_by_role <- function(roles, value) {
  if ("role" %in% names(roles)) {
    v <- roles$variable[roles$role == value]
    if (length(v)) return(v[1])
  }
  if (identical(value, "exposure") && !is.null(roles$is_exposure) && any(roles$is_exposure))
    return(roles$variable[roles$is_exposure][1])
  if (identical(value, "outcome")  && !is.null(roles$is_outcome)  && any(roles$is_outcome))
    return(roles$variable[roles$is_outcome][1])
  NA_character_
}

# Accept ... and paste internally so you can pass multiple args. 
# this is for the pretty colors
.clr_wrap <- function(prefix, suffix) {
  force(prefix); force(suffix)
  function(...) paste0(prefix, paste0(..., collapse = ""), suffix)
}

clr_red    <- .clr_wrap("\033[31m", "\033[39m")
clr_green  <- .clr_wrap("\033[32m", "\033[39m")
clr_yellow <- .clr_wrap("\033[33m", "\033[39m")
clr_blue   <- .clr_wrap("\033[34m", "\033[39m")
clr_bold   <- .clr_wrap("\033[1m",  "\033[22m")

########################## LEGACY HELPER FUNCTIONS #############################
# Pretty, engine-agnostic model comparison table.
# Uses modelsummary if available; else falls back to broom; else coef() head.
# added canonical models
.print_model_comparison <- function(m_orig, m_min, m_canon) {
  # Preferred path: modelsummary
  if (requireNamespace("modelsummary", quietly = TRUE)) {
    mods <- list("Original" = m_orig, "Minimal" = m_min, "Canonical" = m_canon)
    ok <- try({
      tab <- modelsummary::msummary(
        mods,
        stars = TRUE,
        output   = "markdown",
        gof_omit = "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE|Within|Between|Std|sigma"
      )
      cat("\nModel comparison:\n")
      if (is.character(tab)) {
        cat(paste(tab, collapse = "\n"))
      } else {
        print(tab)  # print because cat can't handle tables
      }
    }, silent = TRUE)
    if (!inherits(ok, "try-error")) return(invisible(NULL))
  }
  
  # Fallback: broom
  if (requireNamespace("broom", quietly = TRUE)) {
    cat("\nModel comparison: (fallback; install {modelsummary} for a nicer table):\n")
    show <- function(m, label) {
      if (inherits(m, "DAGassist_fit_error")) {
        cat("\n", label, " (fit error): ", m$error, "\n", sep = "")
        return()
      }
      tt <- tryCatch(broom::tidy(m), error = function(e) NULL)
      if (is.null(tt)) {
        cat("\n", label, ": could not be tidied.\n", sep = ""); return()
      }
      cols <- intersect(c("term","estimate","std.error","statistic","p.value"), names(tt))
      cat("\n", label, ":\n", sep = ""); print(utils::head(tt[, cols, drop = FALSE], 10))
    }
    show(m_orig, "Original"); show(m_min, "Minimal"); show(m_canon, "Canonical")
    return(invisible(NULL))
  }
  
  # Last resort
  cat("\nModel comparison (basic coefficients):\n")
  cat("\nThis is a fallback; install {modelsummary} for a nicer table):\n")
  cat("\nOriginal (coef head):\n"); print(utils::head(stats::coef(m_orig)))
  cat("\nMinimal  (coef head):\n"); print(utils::head(stats::coef(m_min)))
  invisible(NULL)
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
#' @param verbose default TRUE-- TRUE/FALSE -- suppresses formulas and notes.
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
                       engine = stats::lm, engine_args = list(),
                       verbose = TRUE) {
  
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
  
  
  # all the minimal sets
  minimal_sets_all <- .minimal_sets_all(dag, exposure, outcome)
  # keep a single minimal for reference
  minimal <- if (length(minimal_sets_all)) minimal_sets_all[[1]] else character(0)
  
  # formulas for all minimal sets---preserve fixest tails
  f_mins <- lapply(minimal_sets_all, function(s) .build_formula_with_controls(formula, exposure, outcome, s))
  
  # canonical set and formula
  canonical <- .pick_canonical_controls(dag, exposure, outcome)
  f_canon   <- .build_formula_with_controls(formula, exposure, outcome, canonical)
  
  #fits: always show Original, every Minimal, and Canonical at the end 
  #if there are multiple min, push canonical to the end
  m_orig <- .safe_fit(engine, formula, data, engine_args)
  
  # fit each Minimal, reusing m_orig if identical
  m_mins <- lapply(f_mins, function(fm) {
    if (.same_formula(fm, formula)) m_orig else .safe_fit(engine, fm, data, engine_args)
  })
  
  # fit Canonical, reusing an existing fit if identical to Original or a Minimal_i
  if (.same_formula(f_canon, formula)) {
    m_canon <- m_orig
  } else {
    same_idx <- which(vapply(f_mins, function(fm) .same_formula(fm, f_canon), logical(1)))
    m_canon <- if (length(same_idx)) m_mins[[same_idx[1]]] else .safe_fit(engine, f_canon, data, engine_args)
  }
  
  # mark canonical in roles table
  if (is.data.frame(roles) && "variable" %in% names(roles)) {
    roles$canon <- ifelse(roles$variable %in% canonical, "x", "")
  }
  
  out <- list(
    validation = v, 
    roles = roles,
    bad_in_user = bad_in_user,
    controls_minimal = minimal, # keeps legacy single-min key
    controls_minimal_all = minimal_sets_all, # all minimal sets
    controls_canonical = canonical,
    formulas = list(
      original = formula,
      # so it prints multiple formulas if there are multiple minimal sets
      minimal = if (length(f_mins)) f_mins[[1]] else .build_minimal_formula(formula, exposure, outcome, minimal),
      minimal_list = f_mins, #  all minimal formulas
      canonical = f_canon
    ),
    models = list(
      original= m_orig,
      minimal = if (length(m_mins)) m_mins[[1]] else .safe_fit(engine, .build_minimal_formula(formula, exposure, outcome, minimal), data, engine_args),
      minimal_list = m_mins, # all minimal fits
      canonical = m_canon
    ),
    verbose = isTRUE(verbose)
  )
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
  
  cat(clr_bold(clr_blue("DAGassist Report:")), "\n")
  #cat("Validation: ", if (x$validation$ok) "VALID" else "INVALID", "\n", sep = "")
  if (!x$validation$ok) { print(x$validation); return(invisible(x)) }
  
  #set verbose flag for suppressing certain parts
  verbose <- if (is.null(x$verbose)) TRUE else isTRUE(x$verbose)
  
  cat("\nRoles:\n")
  print(x$roles)  # your pretty roles table
  
  if (length(x$bad_in_user)) {
    cat(clr_red("\n (!) Bad controls in your formula: {", paste(x$bad_in_user, collapse = ", "), "}\n", sep = ""))
  } else {
    cat("\nNo bad controls detected in your formula.\n")
  }
  
  # compare adjsutment sets
  if (length(x$controls_minimal_all)) {
    for (i in seq_along(x$controls_minimal_all)) {
      cat("Minimal controls ", i, ": ", .format_set(x$controls_minimal_all[[i]]), "\n", sep = "")
    }
  } else {
    cat("Minimal controls 1: {}\n")
  }
  cat("Canonical controls: ", .format_set(x$controls_canonical), "\n", sep = "")
  
  if(verbose){
  # compare formulas
    cat("\nFormulas:\n", sep = "")
    cat("  original:  ",  deparse(x$formulas$original),  "\n", sep = "")
    if (length(x$formulas$minimal_list)) {
      for (i in seq_along(x$formulas$minimal_list)) {
        cat("  minimal ", sprintf("%-2d", i), ": ", deparse(x$formulas$minimal_list[[i]]), "\n", sep = "")
      }
    } else {
     cat("  minimal 1: ", deparse(x$formulas$minimal), "\n", sep = "")
    }
    cat("  canonical: ",  deparse(x$formulas$canonical), "\n", sep = "")
  
    ## Note if specs are identical (check all pairs and sets)
    mins_fmls <- if (length(x$formulas$minimal_list)) x$formulas$minimal_list else list(x$formulas$minimal)
    #initialize empty value for later
    pairs <- character(0)
  
    # original vs each minimal 
    for (i in seq_along(mins_fmls)) {
      if (.same_formula(x$formulas$original, mins_fmls[[i]])) {
        pairs <- c(pairs, sprintf("Original = Minimal %d", i))
      }
    }
  
    # canonical vs original
    if (.same_formula(x$formulas$canonical, x$formulas$original)) {
      pairs <- c(pairs, "Canonical = Original")
    }
  
  # canonical vs each minimal 
    for (i in seq_along(mins_fmls)) {
      if (.same_formula(x$formulas$canonical, mins_fmls[[i]])) {
        pairs <- c(pairs, sprintf("Canonical = Minimal %d", i))
      }
    }
  
  # minimal vs minimal 
    if (length(mins_fmls) > 1) {
      for (i in seq_len(length(mins_fmls) - 1L)) {
        for (j in seq.int(i + 1L, length(mins_fmls))) {
          if (.same_formula(mins_fmls[[i]], mins_fmls[[j]])) {
            pairs <- c(pairs, sprintf("Minimal %d = Minimal %d", i, j))
          }
        }
      }
    }
  
    if (length(pairs)) {
      cat(clr_yellow("\nNote: some specifications are identical (",
          paste(pairs, collapse = "; "),
          ").\nEstimates will match for those columns.\n", sep = ""))
    }
  
    ## Concise note about DAG-derived additions 
  
    # user RHS terms from the original pre-| formula
    user_rhs <- .rhs_terms_safe(x$formulas$original)
  
    exp_nm <- get_by_role(x$roles, "exposure")
    out_nm <- get_by_role(x$roles, "outcome")
  
    # build one short line per column that added variables
    lines <- character(0)
    drop_exp <- if (!is.na(exp_nm) && nzchar(exp_nm)) exp_nm else character(0)
  
    mins_fmls <- if (length(x$formulas$minimal_list)) x$formulas$minimal_list else list(x$formulas$minimal)
    for (i in seq_along(mins_fmls)) {
      rhs_i  <- setdiff(.rhs_terms_safe(mins_fmls[[i]]), drop_exp)
      added  <- setdiff(rhs_i, user_rhs)
      if (length(added)) lines <- c(lines, sprintf("  - Minimal %d added: %s", i, .format_set(added)))
     }
  
    rhs_c    <- setdiff(.rhs_terms_safe(x$formulas$canonical), drop_exp)
    added_c  <- setdiff(rhs_c, user_rhs)
    if (length(added_c)) lines <- c(lines, sprintf("  - Canonical added: %s", .format_set(added_c)))
  
    if (length(lines)) {
      if (!is.na(exp_nm) && nzchar(exp_nm) && !is.na(out_nm) && nzchar(out_nm)) {
        cat("\nNote: DAGassist added variables not in your formula, based on the\nrelationships in your DAG, ",
            "to block back-door paths\nbetween ", exp_nm, " and ", out_nm, ".\n", sep = "")
      } else {
        cat(clr_bold("\nNote: DAGassist added variables not in your formula, based on the\nrelationships in your DAG, ",
            "to block back-door paths.\n", sep = ""))
      }
      cat(paste(lines, collapse = "\n"), "\n", sep = "")
    }
  }
  ## build the model list for modelsummary/broom
  mods <- list("Original" = x$models$original)
  if (length(x$models$minimal_list)) {
    for (i in seq_along(x$models$minimal_list)) {
      mods[[sprintf("Minimal %d", i)]] <- x$models$minimal_list[[i]]
    }
  } else {
    mods[["Minimal 1"]] <- x$models$minimal
  }
  mods[["Canonical"]] <- x$models$canonical
  
  .print_model_comparison_list(mods)
}