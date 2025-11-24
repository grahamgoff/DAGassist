# R/weights.R — estimand recovery helpers (internal)
# Binary exposures only; console output only.

# Internal: map a model column name to its formula in a DAGassist_report
.dagassist_formula_for_model_name <- function(x, model_name) {
  # x is a DAGassist_report
  
  # 1) Core specs
  if (identical(model_name, "Original")) {
    return(x$formulas$original)
  }
  
  if (identical(model_name, "Bivariate")) {
    return(x$formulas$bivariate)
  }
  
  if (startsWith(model_name, "Minimal ")) {
    idx <- suppressWarnings(as.integer(sub("Minimal\\s+", "", model_name)))
    if (!is.na(idx) && length(x$formulas$minimal_list) >= idx) {
      return(x$formulas$minimal_list[[idx]])
    }
    # fallback: single minimal formula
    return(x$formulas$minimal)
  }
  
  if (identical(model_name, "Canonical")) {
    return(x$formulas$canonical)
  }
  
  # 2) Canonical with exclusions (Canon. (-NCT), Canon. (-NCO), etc.)
  cex <- x$formulas$canonical_excl
  if (!is.null(cex)) {
    if (is.list(cex)) {
      # new style: list(nco = <fml>, nct = <fml>, ...)
      for (nm in names(cex)) {
        lbl <- switch(
          nm,
          nct = "Canon. (-NCT)",
          nco = "Canon. (-NCO)",
          paste0("Canonical (", nm, ")")
        )
        if (identical(lbl, model_name)) return(cex[[nm]])
      }
    } else {
      # old style: single canonical_excl
      # All labels ("Canon. (-NCT)", etc.) map to same formula
      return(cex)
    }
  }
  
  NULL
}

# Internal: choose controls for the treatment model
.dagassist_treatment_controls <- function(x, exposure) {
  # Prefer canonical controls if available
  controls <- x$controls_canonical
  if (length(controls)) {
    controls <- unname(controls)
  } else {
    # fall back to the first minimal set, then to collapsed minimal
    if (length(x$controls_minimal_all)) {
      controls <- x$controls_minimal_all[[1L]]
    } else if (length(x$controls_minimal)) {
      controls <- x$controls_minimal
    } else {
      # last resort: RHS of original, minus exposure/outcome
      rhs <- .rhs_terms_safe(x$formulas$original)
      out <- get_by_role(x$roles, "outcome")
      controls <- setdiff(rhs, c(exposure, out))
    }
  }
  
  # keep only variables actually in the data
  controls <- intersect(controls, names(x$.__data))
  unique(controls)
}

# Internal: add weighted versions of each model column (ATE/ATT) using WeightIt
.dagassist_add_weighted_models <- function(x, mods) {
  # x: DAGassist_report
  # mods: named list of fitted models (Original, Minimal 1, Canonical, ...)
  
  est <- x$settings$estimand
  if (is.null(est) || identical(est, "none")) {
    return(mods)
  }
  
  # require WeightIt
  if (!requireNamespace("WeightIt", quietly = TRUE)) {
    stop(
      "Estimand recovery (estimand = '", est, "') requires the 'WeightIt' package.\n",
      "Please install WeightIt or set estimand = 'none'.",
      call. = FALSE
    )
  }
  
  data <- x$.__data
  if (is.null(data)) {
    stop("Original data not available in report; estimand recovery requires DAGassist() to be called with `data=`.", call. = FALSE)
  }
  
  # exposure name from roles
  exp_nm <- get_by_role(x$roles, "exposure")
  if (is.na(exp_nm) || !nzchar(exp_nm)) {
    stop("Could not identify a unique exposure node; estimand recovery requires a single exposure.", call. = FALSE)
  }
  if (!exp_nm %in% names(data)) {
    stop("Exposure variable '", exp_nm, "' not found in data; cannot compute weights.", call. = FALSE)
  }
  
  # binary check: allow 0/1 numeric or 2-level factor
  Tvar <- data[[exp_nm]]
  if (is.factor(Tvar)) {
    if (nlevels(Tvar) != 2L) {
      stop("Estimand recovery currently supports only binary exposures (2-level factor).", call. = FALSE)
    }
  } else {
    uniq <- sort(unique(stats::na.omit(Tvar)))
    if (!all(uniq %in% c(0, 1))) {
      stop("Estimand recovery currently supports only binary exposures coded 0/1 (or a 2-level factor).", call. = FALSE)
    }
  }
  
  # choose controls for treatment model
  controls <- .dagassist_treatment_controls(x, exp_nm)
  
  # build treatment formula
  if (length(controls)) {
    rhs <- paste(controls, collapse = " + ")
    f_treat <- stats::as.formula(paste(exp_nm, "~", rhs))
  } else {
    f_treat <- stats::as.formula(paste(exp_nm, "~ 1"))
  }
  
  # compute weights via WeightIt
  wtobj <- WeightIt::weightit(
    formula  = f_treat,
    data     = data,
    method   = "ps",
    estimand = est
  )
  w <- wtobj$weights
  if (length(w) != nrow(data)) {
    stop(
      "WeightIt returned weights of length ", length(w),
      " for data with ", nrow(data), " rows.",
      call. = FALSE
    )
  }
  
  # engine and engine_args from DAGassist() call, stored in settings
  engine      <- x$settings$engine
  engine_args <- x$settings$engine_args
  if (is.null(engine)) {
    stop(
      "Modeling engine not found on report object.\n",
      "Please ensure DAGassist() stores `engine` in report$settings.",
      call. = FALSE
    )
  }
  if (!is.list(engine_args)) engine_args <- list()
  
  # we want engine_args + weights; weights should override if already present
  engine_args_w <- utils::modifyList(engine_args, list(weights = w))
  
  # fit weighted versions for each model using same formula + data + engine
  weighted_mods <- list()
  for (nm in names(mods)) {
    fml <- .dagassist_formula_for_model_name(x, nm)
    if (is.null(fml)) next
    fit_w <- .safe_fit(engine, fml, data, engine_args_w)
    weighted_mods[[nm]] <- fit_w
  }
  
  # interleave: Original, Original (ATE), Minimal 1, Minimal 1 (ATE), etc.
  est_label <- paste0(" (", est, ")")
  mods_out  <- list()
  
  for (nm in names(mods)) {
    mods_out[[nm]] <- mods[[nm]]
    if (!is.null(weighted_mods[[nm]])) {
      new_name <- paste0(nm, est_label)
      mods_out[[new_name]] <- weighted_mods[[nm]]
    }
  }
  
  mods_out
}