# weights.R
#
# Estimand recovery helpers
#
# These helpers add optional estimand recovery to DAGassist console output.
# When the user sets `estimand = "ATE"` or `"ATT"` (and `type = "console"`),
# DAGassist will:
#   - Pick a set of controls from the report (canonical → minimal → fallback),
#   - Use WeightIt to fit a binary treatment model and compute IPW weights,
#   - Re-fit each comparison model with those weights, and
#   - Append the weighted models as extra columns in the console table
#      (e.g., "Original (ATE)", "Minimal 1 (ATE)", "Canonical (ATE)", ...).
#
# Design choices:
#   - Binary exposures only (0/1 or two-level factors),
#   - Console output only (no LaTeX/Word/Excel integration yet),
#   - WeightIt is optional; if it isn't installed, we fail with a clear message.

# Decide whether exposure is binary (0/1 numeric, logical, or 2-level factor)
# vs continuous (numeric with >= 3 unique values).
.dagassist_exposure_kind <- function(Tvar) {
  if (is.logical(Tvar)) return("binary")
  
  if (is.factor(Tvar)) {
    if (nlevels(Tvar) == 2L) return("binary")
    return("unsupported")
  }
  
  if (is.numeric(Tvar)) {
    u <- sort(unique(stats::na.omit(Tvar)))
    if (length(u) >= 3L) return("continuous")
    if (length(u) == 2L && all(u %in% c(0, 1))) return("binary")
    return("unsupported")  # e.g., 1/2 coding -> require recode to 0/1
  }
  
  "unsupported"
}

# Allow user to write stop_method or stop.method; normalize to twangContinuous’s stop.method
.dagassist_normalize_weights_args <- function(args) {
  if (is.null(args)) return(list())
  if (!is.list(args)) stop("`weights_args` must be a list.", call. = FALSE)
  
  if ("stop_method" %in% names(args) && !"stop.method" %in% names(args)) {
    names(args)[names(args) == "stop_method"] <- "stop.method"
  }
  args
}

# Keep only args that are formal arguments of `fun`; return list(keep=..., drop=...)
.dagassist_filter_args <- function(args, fun) {
  if (length(args) == 0L) return(list(keep = list(), drop = character(0)))
  
  nms <- names(args)
  if (is.null(nms) || any(nms == "")) {
    stop("`weights_args` must be a *named* list.", call. = FALSE)
  }
  
  fmls <- names(formals(fun))
  keep_nms <- intersect(nms, fmls)
  drop_nms <- setdiff(nms, keep_nms)
  
  list(keep = args[keep_nms], drop = drop_nms)
}

# support multicolumn weight output
.dagassist_formula_for_model_name <- function(x, model_name) {
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
    # Fallback: single minimal formula
    return(x$formulas$minimal)
  }
  
  if (identical(model_name, "Canonical")) {
    return(x$formulas$canonical)
  }
  
  # Canonical with exclusions (e.g., "Canon. (-NCT)", "Canon. (-NCO)", ...)
  cex <- x$formulas$canonical_excl
  if (!is.null(cex)) {
    if (is.list(cex)) {
      # New style: list(nco = <fml>, nct = <fml>, ...)
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
      # Old style: single canonical_excl
      # All labels ("Canon. (-NCT)", etc.) map to the same formula
      return(cex)
    }
  }
  
  # If we get here, we couldn't match the label to a stored formula
  NULL
}

# Choose controls for the treatment model
.dagassist_treatment_controls <- function(x, exposure) {
  # find control set for the treatment model:
  #   - Prefer canonical controls
  #   - Otherwise use the first minimal set
  #   - Otherwise fall back to the original controls (excluding exposure/outcome)
  
  controls <- x$controls_canonical
  if (length(controls)) {
    controls <- unname(controls)
  } else {
    if (length(x$controls_minimal_all)) {
      controls <- x$controls_minimal_all[[1L]]
    } else if (length(x$controls_minimal)) {
      controls <- x$controls_minimal
    } else {
      # Last resort: RHS of original formula, minus exposure and outcome
      rhs <- .rhs_terms_safe(x$formulas$original)
      out <- get_by_role(x$roles, "outcome")
      controls <- setdiff(rhs, c(exposure, out))
    }
  }
  # keep only variables that are actually present in the data
  controls <- intersect(controls, names(x$.__data))
  unique(controls)
}

# Add weighted versions of each model column (ATE/ATT) using WeightIt
.dagassist_add_weighted_models <- function(x, mods) {
  est <- x$settings$estimand
  if (is.null(est) || identical(est, "none")) {
    return(mods)
  }
  
  data <- x$.__data
  if (is.null(data)) {
    stop(
      "Original data not found on the report object.\n",
      "Estimand recovery requires calling DAGassist() with the `data` argument.",
      call. = FALSE
    )
  }
  
  exp_nm <- get_by_role(x$roles, "exposure")
  
  # only support a single binary exposure for estimand recovery. this will mostly
  # apply to diff in diff. may need to add support to picking one exposure to 
  # weight on (likely, the non-time one) after researching
  if (length(exp_nm) != 1L || is.na(exp_nm) || !nzchar(exp_nm)) {
    stop(
      "Estimand recovery currently supports exactly one exposure node.\n",
      "DAGassist found ", length(exp_nm), " exposure nodes in the DAG.\n\n",
      "Please either:\n",
      "  * simplify the DAG to a single exposure for this call, or\n",
      "  * set `estimand = \"none\"`.\n",
      call. = FALSE
    )
  }
  
  if (!exp_nm %in% names(data)) {
    stop(
      "Exposure variable '", exp_nm, "' was identified in the DAG but not found in `data`.\n",
      "Please check that the DAG node names and data column names match.",
      call. = FALSE
    )
  }
  
  Tvar <- data[[exp_nm]]
  kind <- .dagassist_exposure_kind(Tvar)
  
  # normalize and read weight args 
  wargs <- .dagassist_normalize_weights_args(x$settings$weights_args)
  
  if (identical(kind, "binary")) {
    if (!requireNamespace("WeightIt", quietly = TRUE)) {
      stop(
        "Estimand recovery for binary exposures requires the 'WeightIt' package.\n",
        "Install it (install.packages('WeightIt')) or set estimand = 'none'.",
        call. = FALSE
      )
    }
  } else if (identical(kind, "continuous")) {
    if (!requireNamespace("twangContinuous", quietly = TRUE)) {
      stop(
        "Estimand recovery for continuous exposures requires the 'twangContinuous' package.\n",
        "Install it (install.packages('twangContinuous')) or set estimand = 'none'.",
        call. = FALSE
      )
    }
    if (!identical(est, "ATE")) {
      stop(
        "For continuous exposures, DAGassist currently supports estimand = 'ATE' only.\n",
        "Set estimand = 'ATE' (or 'none').",
        call. = FALSE
      )
    }
  } else {
    # helpful error
    u <- try(sort(unique(stats::na.omit(Tvar))), silent = TRUE)
    stop(
      "Estimand recovery supports either:\n",
      "  * Binary exposures (0/1 numeric, logical, or 2-level factor), or\n",
      "  * Continuous numeric exposures (>= 3 unique values).\n\n",
      "Exposure '", exp_nm, "' is class: ", paste(class(Tvar), collapse = "/"),
      if (!inherits(u, "try-error")) paste0("\nObserved values (unique): ", paste(u, collapse = ", ")) else "",
      "\n\nPlease recode it or set estimand = 'none'.",
      call. = FALSE
    )
  }
  
  # choose controls for the treatment model
  controls <- .dagassist_treatment_controls(x, exp_nm)
  
  # build treatment model: exposure ~ controls
  if (length(controls)) {
    rhs    <- paste(controls, collapse = " + ")
    f_treat <- stats::as.formula(paste(exp_nm, "~", rhs))
  } else {
    # no controls: simple treated vs not-treated
    f_treat <- stats::as.formula(paste(exp_nm, "~ 1"))
  }
  
  # Compute weights via the appropriate backend
  if (identical(kind, "binary")) {
    # filter user-provided args to what weightit() actually accepts
    fa <- .dagassist_filter_args(wargs, WeightIt::weightit)
    if (length(fa$drop)) {
      warning(
        "Ignoring these weights_args for WeightIt::weightit(): ",
        paste(fa$drop, collapse = ", "),
        call. = FALSE
      )
    }
    
    wtobj <- do.call(
      WeightIt::weightit,
      c(
        list(
          formula = f_treat,
          data = data,
          method = "ps",
          estimand = est
        ),
        fa$keep
      )
    )
    
    w <- wtobj$weights
    
  } else if (identical(kind, "continuous")) {
    # filter args to what ps.cont() accepts
    fa <- .dagassist_filter_args(wargs, twangContinuous::ps.cont)
    if (length(fa$drop)) {
      warning(
        "Ignoring these weights_args for twangContinuous::ps.cont(): ",
        paste(fa$drop, collapse = ", "),
        call. = FALSE
      )
    }
    
    psobj <- do.call(
      twangContinuous::ps.cont,
      c(list(formula = f_treat, data = data), fa$keep)
    )
    
    # Use the same stop.method if user provided it; else twangContinuous default ("wcor")
    stop_method <- if (!is.null(fa$keep[["stop.method"]])) fa$keep[["stop.method"]] else "wcor"
    
    w <- twangContinuous::get.weights(psobj, stop.method = stop_method, withSampW = TRUE)
  }
  
  if (length(w) != nrow(data)) {
    stop(
      "WeightIt returned ", length(w), " weights for data with ",
      nrow(data), " rows.\n",
      "Please inspect the WeightIt object and your treatment model.",
      call. = FALSE
    )
  }
  
  # engine and engine_args from DAGassist() call, stored in settings
  engine <- x$settings$engine
  engine_args <- x$settings$engine_args
  
  if (is.null(engine)) {
    stop(
      "Modeling engine not found on the report object.\n",
      "Please ensure DAGassist() stores `engine` in report$settings.",
      call. = FALSE
    )
  }
  
  if (!is.list(engine_args)) engine_args <- list()
  
  # add weights to engine_args (weights override any existing entry)
  engine_args_w <- utils::modifyList(engine_args, list(weights = w))
  
  # fit weighted versions for each model using the same engine 
  weighted_mods <- list()
  for (nm in names(mods)) {
    fml <- .dagassist_formula_for_model_name(x, nm)
    if (is.null(fml)) next
    fit_w <- .safe_fit(engine, fml, data, engine_args_w)
    weighted_mods[[nm]] <- fit_w
  }
  
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