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
#' @param type Output type: "console" (default) or "latex" or "word"
#' @param out File path for latex fragment or word doc, default NULL
#' @param imply logical. If TRUE, DAGassist will add variables not included in
#' the user's main formula, based on the relationships in the DAG. If FALSE (default),
#' only the original model is shown
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
#' DAGassist(test_complex, Y ~ X + Z + C + M, test_df, exposure = "X", outcome = "Y")
#' 
#' @export

DAGassist <- function(dag, formula, data, exposure, outcome,
                       engine = stats::lm, engine_args = list(),
                       verbose = TRUE, 
                       type = c("console", "latex", "word", "docx", 
                                "excel", "xlsx", "text", "txt"), 
                       out = NULL,
                       imply = FALSE) {
  # set output type
  type <- match.arg(type)
  
  ## allow formula to be either a formula or a single engine call
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
  
  ## infer exposure/outcome from DAG if user didn't set them
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
  
  ## if auto add is true, imply the sets based on dag relationships,
  ## otherwise, keep lists empty and specify in the report list
  if (isTRUE(imply)) {
    if (is.data.frame(roles) && "variable" %in% names(roles)) {
      roles$canon <- ifelse(roles$variable %in% canonical, "x", "")
    }
  } else {
    if (is.data.frame(roles) && "canon" %in% names(roles)) roles$canon <- ""
  }
  
  # ---- ALWAYS fit alternates so display isn't tied to `imply` ----
  m_mins <- lapply(
    f_mins,
    function(fm) if (.same_formula(fm, formula)) m_orig else .safe_fit(engine, fm, data, engine_args)
  )
  
  m_canon <- if (.same_formula(f_canon, formula)) {
    m_orig
  } else {
    same_idx <- which(vapply(f_mins, function(fm) .same_formula(fm, f_canon), logical(1)))
    if (length(same_idx)) m_mins[[same_idx[1]]] else .safe_fit(engine, f_canon, data, engine_args)
  }
  # ----------------------------------------------------------------
  
  
  #8/28/25 renaming to "report", a more suitable name, to avoid naming conflict
  # with out param
  report <- list(
    validation = v, 
    roles = roles,
    bad_in_user = bad_in_user,
    controls_minimal = minimal, # keeps legacy single-min key
    controls_minimal_all = minimal_sets_all, # all minimal sets
    controls_canonical = canonical,
    formulas = list(
      original = formula,
      # so it prints multiple formulas if there are multiple minimal sets
      minimal = if (length(f_mins)) f_mins[[1]] else if (isTRUE(imply)) .build_minimal_formula(formula, exposure, outcome, minimal) else formula,
      minimal_list = f_mins, #  all minimal formulas
      canonical = f_canon
    ),
    models = list(
      original= m_orig,
      minimal = if (length(m_mins)) m_mins[[1]] else if (isTRUE(imply)) .safe_fit(engine, .build_minimal_formula(formula, exposure, outcome, minimal), data, engine_args) else m_orig,
      minimal_list = m_mins, # all minimal fits
      canonical = m_canon
    ),
    verbose = isTRUE(verbose),
    imply = isTRUE(imply)
  )
  class(report) <- c("DAGassist_report", class(report))
  # Build unified artifacts once for all outputs
  mods_full       <- .build_named_mods(report)
  models_df_full  <- .build_models_df(report)
  ##### LATEX OUT BRANCH #####
  if (type == "latex") {
    if (tolower(tools::file_ext(out)) == "docx") {
      stop("LaTeX fragment must not be written to a .docx path. Use a .tex filename.", call. = FALSE)
    }
    if (is.null(out)) stop("type='latex' requires `out=` file path.", call. = FALSE)
    
    res_min <- list(
      validation = list(
        status = if (isTRUE(v$ok)) "VALID" else "INVALID",
        issues = if (!is.null(v$issues)) v$issues else character(0)
      ),
      roles_df  = report$roles,
      models_df = models_df_full,     # <— unified
      models    = mods_full,          # <— unified
      min_sets  = report$controls_minimal_all,
      canon     = report$controls_canonical
    )
    
    .report_latex_fragment(res_min, out)
    return(invisible(structure(report, file = normalizePath(out, mustWork = FALSE))))
  }
  ##### WORD OUT BRANCH #####
  if (type %in% c("docx","word")) {
    res_min <- list(
      validation = list(
        status = if (isTRUE(v$ok)) "VALID" else "INVALID",
        issues = if (!is.null(v$issues)) v$issues else character(0)
      ),
      roles_df = report$roles,
      models   = mods_full,                 # <— unified (always Original + Minimal(s) + maybe Canonical)
      min_sets = report$controls_minimal_all,
      canon    = report$controls_canonical
    )
    return(.report_docx(res_min, out))
  }
  
  #### EXCEL OUT BRANCH ####
  if (type %in% c("excel","xlsx")) {
    res_min <- list(
      roles_df = report$roles,
      models   = mods_full,          # <— unified
      min_sets = report$controls_minimal_all,
      canon    = report$controls_canonical
    )
    .report_xlsx(res_min, out)
    return(invisible(structure(report, file = normalizePath(out, mustWork = FALSE))))
  }
  
  ##### TEXT OUT BRANCH #####
  if (type %in% c("text","txt")) {
    res_min <- list(
      roles_df = report$roles,
      models   = mods_full,          # <— unified
      min_sets = report$controls_minimal_all,
      canon    = report$controls_canonical
    )
    .report_txt(res_min, out)
    return(invisible(structure(report, file = normalizePath(out, mustWork = FALSE))))
  }
  
  report
}

#' print the DAGassist_report
#' @param x Output of DAGassist() (class "report")
#' @param ... Additional arguments passed to or from methods (unused).
#' @return Invisibly returns x
#' 
#' @examplesIf requireNamespace("dagitty", quietly = TRUE)
#' # Use package datasets to avoid re-simulating
#' data(test_df, package = "DAGassist")
#' data(test_complex, package = "DAGassist")
#' DAGassist(test_complex, Y ~ X + Z + C + M, test_df, exposure = "X", outcome = "Y")
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
    cat(clr_green("\nNo bad controls detected in your formula.\n"))
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
    cat(clr_bold("\nFormulas:\n", sep = ""))
    cat("  original:  ",  deparse(x$formulas$original),  "\n", sep = "")
    if (isTRUE(x$imply)) {
      
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
          cat(clr_bold(clr_yellow("\nNote: DAGassist added variables not in your formula, based on the\nrelationships in your DAG, ",
              "to block back-door paths\nbetween ", exp_nm, " and ", out_nm, ".\n", sep = "")))
        } else {
          cat(clr_bold(clr_yellow("\nNote: DAGassist added variables not in your formula, based on the\nrelationships in your DAG, ",
              "to block back-door paths.\n", sep = "")))
        }
        cat(paste(lines, collapse = "\n"), "\n", sep = "")
      }
    }
  } # end of auto add
  
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