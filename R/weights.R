# weights.R
#
# Estimand recovery helpers

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

.dagassist_formula_for_model_name <- function(x, model_name) {
  
  # detect derived suffix
  is_weighted <- grepl("\\((ATE|ATT)\\)\\s*$", model_name, ignore.case = TRUE)
  is_acde <- grepl("\\((ACDE|CDE)\\)\\s*$", model_name, ignore.case = TRUE)
  
  base_name <- sub("\\s*\\((ATE|ATT|ACDE|CDE)\\)\\s*$", "", model_name, ignore.case = TRUE)
  
  # If ACDE model label, build sequential_g formula from the *base* model formula
  if (is_acde) {
    base_fml <- .dagassist_formula_for_model_name(x, base_name)
    if (is.null(base_fml)) return(NULL)
    acde <- .dagassist_normalize_acde_spec(x$settings$acde)
    return(.dagassist_build_acde_formula(base_fml, x, acde))
  }
  
  # weighted models use the same regression formula as their base model
  if (is_weighted) {
    return(.dagassist_formula_for_model_name(x, base_name))
  }
  
  # ---- existing mapping logic (unchanged), but applied to base_name ----
  # NOTE: this is your previous body, adjusted to use `base_name` instead of `model_name`
  
  of <- x$formulas$original
  bf <- x$formulas$bivariate
  mf <- x$formulas$minimal
  cf <- x$formulas$canonical
  cex <- x$formulas$canonical_excl
  
  if (identical(base_name, "Original")) return(of)
  if (identical(base_name, "Bivariate")) return(bf)
  
  # Minimal i variants
  if (grepl("^Minimal\\s+[0-9]+$", base_name)) {
    idx <- as.integer(sub("^Minimal\\s+", "", base_name))
    if (length(x$formulas$minimal_list) && idx <= length(x$formulas$minimal_list)) {
      return(x$formulas$minimal_list[[idx]])
    }
    return(mf)
  }
  
  if (identical(base_name, "Canonical")) return(cf)
  
  # Canonical exclusion variants
  if (!is.null(cex)) {
    # list of excluded variants
    if (is.list(cex)) {
      for (nm in names(cex)) {
        lbl <- switch(
          nm,
          nct = "Canon. (-NCT)",
          nco = "Canon. (-NCO)",
          paste0("Canonical (", nm, ")")
        )
        if (identical(lbl, base_name)) return(cex[[nm]])
      }
    } else {
      # old single-model behavior; map any label to the same formula
      if (base_name %in% c("Canon. (-NCT)", "Canon. (-NCO)") || grepl("^Canonical\\s*\\(", base_name)) {
        return(cex)
      }
    }
  }
  
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

# ---- Estimand normalization (supports vectors) ----
.dagassist_normalize_estimand <- function(estimand) {
  if (is.null(estimand)) return("RAW")
  est <- toupper(as.character(estimand))
  est <- match.arg(est,
                   choices = c("RAW","NONE","ATE","ATT","ACDE","CDE"),
                   several.ok = TRUE)
  est[est == "NONE"] <- "RAW"
  est[est == "CDE"]  <- "ACDE"
  unique(est)
}

# ---- Normalize ACDE spec list ----
.dagassist_normalize_acde_spec <- function(acde) {
  if (is.null(acde)) acde <- list()
  if (!is.list(acde)) stop("`acde` must be a list.", call. = FALSE)
  defaults <- list(
    m = NULL,                 # mediators (character)
    x = NULL,                 # baseline covariates override (character)
    z = NULL,                 # intermediate covariates override (character)
    fe = NULL,                # fixed-effects vars override (character)
    fe_as_factor = TRUE,      # wrap FE vars as factor()
    include_descendants = FALSE  # treat Dmediator as mediators
  )
  # base R merge
  out <- defaults
  for (nm in names(acde)) out[[nm]] <- acde[[nm]]
  out
}

# ---- Detect if author formula conditions on mediator(s) ----
.dagassist_formula_controls_mediator <- function(formula, roles, include_descendants = FALSE) {
  rhs <- .rhs_terms_safe(formula)
  meds <- roles$variable[roles$role == "mediator"]
  if (isTRUE(include_descendants)) {
    meds <- unique(c(meds, roles$variable[roles$role == "Dmediator"]))
  }
  length(intersect(rhs, meds)) > 0L
}

.dagassist_apply_auto_acde <- function(estimand,
                                       formula,
                                       roles,
                                       auto_acde = TRUE,
                                       include_descendants = FALSE) {
  if (!isTRUE(auto_acde)) return(estimand)
  
  ests <- unique(.dagassist_normalize_estimand(estimand))
  wants_total <- any(ests %in% c("ATE", "ATT"))
  if (!wants_total) return(estimand)
  
  controls_mediator <- .dagassist_formula_controls_mediator(
    formula,
    roles,
    include_descendants = include_descendants
  )
  if (!isTRUE(controls_mediator)) return(estimand)
  
  # identify mediators in  RHS 
  meds <- roles$variable[roles$is_mediator |
                           (isTRUE(include_descendants) & roles$is_descendant_of_mediator)]
  meds_in_rhs <- intersect(.rhs_terms_safe(formula), meds)
  
  stop(
    paste0(
      "You requested estimand = ", paste(sprintf("'%s'", intersect(ests, c("ATE","ATT"))), collapse = " / "),
      ", which targets a total effect.\n",
      "However, your model conditions on mediator term(s): {",
      paste(meds_in_rhs, collapse = ", "),
      "}.\n\n",
      "Fix options:\n",
      "  1) Remove mediator terms from the formula and keep estimand = 'ATE'/'ATT' (total effect), OR\n",
      "  2) Set estimand = 'ACDE' (alias: 'CDE') to estimate a controlled direct effect via sequential g-estimation.\n",
      "  3) If you only want the naive regression output with mediators, use estimand = 'RAW'.\n"
    ),
    call. = FALSE
  )
}

.dagassist_safe_descendants <- function(dag, node) {
  tryCatch(dagitty::descendants(dag, node), error = function(e) character(0))
}

.dagassist_safe_ancestors <- function(dag, node) {
  tryCatch(dagitty::ancestors(dag, node), error = function(e) character(0))
}

# ---- Infer mediator terms for ACDE ----
.dagassist_infer_acde_mediators <- function(x, acde) {
  # explicit user override wins
  if (!is.null(acde$m) && length(acde$m)) return(unique(as.character(acde$m)))
  
  # infer from roles (and optionally Dmediator)
  meds <- x$roles$variable[x$roles$role == "mediator"]
  if (isTRUE(acde$include_descendants)) {
    meds <- unique(c(meds, x$roles$variable[x$roles$role == "Dmediator"]))
  }
  
  # if author formula includes some mediators, prioritize those
  rhs <- .rhs_terms_safe(x$formulas$original)
  in_fml <- intersect(rhs, meds)
  if (length(in_fml)) return(in_fml)
  
  # fallback: all DAG mediators
  unique(meds)
}

# Wrap factor() ONLY for bare symbols that exist as columns in `data`.
# This prevents factor(TRUE) and other length-1 constants from ever being created.
.dagassist_factorize_plain_terms <- function(terms, data_names = NULL) {
  if (!length(terms)) return(character(0))
  
  is_bare_symbol <- grepl("^[.A-Za-z][.A-Za-z0-9._]*$", terms)
  
  if (!is.null(data_names)) {
    is_in_data <- terms %in% data_names
    to_wrap <- is_bare_symbol & is_in_data
  } else {
    to_wrap <- is_bare_symbol
  }
  
  terms[to_wrap] <- paste0("factor(", terms[to_wrap], ")")
  terms
}

# Build sequential_g formula (y ~ A + X | Z | M)
.dagassist_build_acde_formula <- function(base_fml, x, acde) {
  if (is.null(x$dag)) {
    stop("ACDE requires storing the evaluated DAG on the report as `x$dag`.", call. = FALSE)
  }
  dag <- x$dag
  exp_nm <- get_by_role(x$roles, "exposure")
  out_nm <- get_by_role(x$roles, "outcome")
  
  # mediators
  m_terms <- .dagassist_infer_acde_mediators(x, acde)
  if (!length(m_terms)) {
    stop("ACDE requested, but no mediator(s) could be inferred. Provide acde = list(m = c('M1','M2')).",
         call. = FALSE)
  }
  
  # strip fixest/random-effects tails from base formula (sequential_g uses | separators)
  sp <- .strip_fixest_parts(base_fml)
  base <- sp$base
  
  rhs_terms <- .rhs_terms_safe(base)  # term labels (includes exposure if present)
  rhs_terms <- unique(rhs_terms)
  
  # Partition RHS into X vs Z unless user overrides
  # Z: descendants of A that are ALSO ancestors of mediator(s) AND outcome (intermediate confounders)
  descA <- .dagassist_safe_descendants(dag, exp_nm)
  ancY  <- .dagassist_safe_ancestors(dag, out_nm)
  ancM  <- unique(unlist(lapply(m_terms, function(m) .dagassist_safe_ancestors(dag, m))))
  
  # exclude mediator terms from candidates and don't let exposure in
  cand <- setdiff(rhs_terms, c(exp_nm, m_terms))
  
  z_auto <- intersect(cand, descA)
  z_auto <- intersect(z_auto, ancY)
  z_auto <- intersect(z_auto, ancM)
  
  # also drop anything that is descendant of mediator (post-mediator)
  descM <- unique(unlist(lapply(m_terms, function(m) .dagassist_safe_descendants(dag, m))))
  z_auto <- setdiff(z_auto, descM)
  
  # user override
  if (!is.null(acde$z)) z_terms <- unique(as.character(acde$z)) else z_terms <- z_auto
  #force exclude exposure from z
  z_terms <- setdiff(z_terms, exp_nm)
  # X: everything else (excluding exposure, mediators, Z)
  x_auto <- setdiff(rhs_terms, c(exp_nm, m_terms, z_terms))
  if (!is.null(acde$x)) x_terms <- unique(as.character(acde$x)) else x_terms <- x_auto
  
  # Append FE vars (engine-agnostic) into the X block.
  # Key rule: only factorize symbols that actually exist in the data.
  data_names <- names(x$.__data)
  # Drop any constant / non-data term strings (this removes "TRUE")
  x_terms <- .dagassist_terms_must_use_data(x_terms, data_names)
  z_terms <- .dagassist_terms_must_use_data(z_terms, data_names)
  m_terms <- .dagassist_terms_must_use_data(m_terms, data_names)
  
  fe_vars <- .dagassist_extract_fe_vars(base_fml)
  if (!is.null(acde$fe) && length(acde$fe)) {
    # allow user override: accept vars or terms; coerce to character and merge
    fe_vars <- unique(c(fe_vars, as.character(acde$fe)))
  }
  #strip any weird junk from acde internal params
  fe_vars <- setdiff(fe_vars, c("TRUE","FALSE","T","F","1","0"))
  if (length(fe_vars)) {
    if (isTRUE(acde$fe_as_factor)) {
      fe_terms <- .dagassist_factorize_plain_terms(fe_vars, data_names = data_names)
    } else {
      fe_terms <- fe_vars
    }
    x_terms <- unique(c(x_terms, fe_terms))
  }
  # Drop any baseline/intermediate covariates that are collinear with FE
  # (e.g., time-invariant within unit when unit FE are included).
  #fixest does this automatically and gracefully; DirectEffects does not. without
  #something like this, collinearity breaks seqg
  if (length(fe_vars)) {
    dx <- .dagassist_drop_terms_collinear_with_fe(x_terms, x$.__data, fe_vars)
    x_terms <- dx$keep
    
    dz <- .dagassist_drop_terms_collinear_with_fe(z_terms, x$.__data, fe_vars)
    z_terms <- dz$keep
    
    if (isTRUE(x$verbose) && length(c(dx$dropped, dz$dropped))) {
      message("Dropped FE-collinear covariates in ACDE: ",
              paste(unique(c(dx$dropped, dz$dropped)), collapse = ", "))
    }
  }
  # Build text formula
  # First block always includes exposure; append X if any
  block1 <- paste(c(exp_nm, x_terms), collapse = " + ")
  if (!nzchar(block1)) block1 <- exp_nm
  ##set to 0 instead of 1 because 1 trips singularity in first stage, causing summary() to fail
  blockZ <- if (length(z_terms)) paste(z_terms, collapse = " + ") else "0"
  blockM <- paste(m_terms, collapse = " + ")
  
  f_txt <- paste0(out_nm, " ~ ", block1, " | ", blockZ, " | ", blockM)
  stats::as.formula(f_txt, env = environment(base_fml))
}

# ---- Add ACDE models (sequential g-estimation) ----
.dagassist_add_acde_models <- function(x, mods) {
  if (!requireNamespace("DirectEffects", quietly = TRUE)) {
    stop(
      "Estimand recovery for ACDE/CDE requires the 'DirectEffects' package.\n",
      "Install it (install.packages('DirectEffects')) or set estimand = 'raw'.",
      call. = FALSE
    )
  }
  
  data <- x$.__data
  if (is.null(data)) {
    stop(
      "Original data not found on the report object.\n",
      "ACDE requires calling DAGassist() with the `data` argument.",
      call. = FALSE
    )
  }
  
  acde <- .dagassist_normalize_acde_spec(x$settings$acde)
  
  # args for sequential_g (user-supplied)
  de_args <- x$settings$directeffects_args
  if (is.null(de_args)) de_args <- list()
  
  out <- mods
  for (nm in names(mods)) {
    # skip already-derived models
    if (grepl("\\((ATE|ATT|ACDE)\\)\\s*$", nm, ignore.case = TRUE)) next
    
    base_fml <- .dagassist_formula_for_model_name(x, nm)
    if (is.null(base_fml)) next
    
    f_acde <- .dagassist_build_acde_formula(base_fml, x, acde)
    
    if (isTRUE(x$verbose)) {
      message("ACDE formula [", nm, "]: ", paste(deparse(f_acde, width.cutoff = 500L), collapse = " "))
    }
    
    fit <- .safe_fit(DirectEffects::sequential_g, f_acde, data, de_args)
    
    out[[paste0(nm, " ", .dagassist_model_name_labels("ACDE"))]] <- fit
  }
  out
}

# add whichever estimands are requested
.dagassist_add_estimand_models <- function(x, mods) {
  ests <- .dagassist_normalize_estimand(x$settings$estimand)
  if (!length(ests) || identical(ests, "RAW")) return(mods)
  
  out <- mods
  # weights first (ATE/ATT)
  if ("ATE" %in% ests) out <- .dagassist_add_weighted_models(x, out, estimand = "ATE")
  if ("ATT" %in% ests) out <- .dagassist_add_weighted_models(x, out, estimand = "ATT")
  
  # ACDE last
  if ("ACDE" %in% ests) out <- .dagassist_add_acde_models(x, out)
  
  out
}


# Add weighted versions of each model column (ATE/ATT) using WeightIt
.dagassist_add_weighted_models <- function(x, mods, estimand = NULL) {
  # Use the estimand requested by the caller (ATE vs ATT). If not provided,
  # fall back to whatever is stored on the report object.
  ests <- .dagassist_normalize_estimand(
    if (!is.null(estimand)) estimand else x$settings$estimand
  )
  
  # Weighting only applies to ATE/ATT. ACDE is handled elsewhere.
  ests <- intersect(ests, c("ATE", "ATT"))
  if (!length(ests)) return(mods)
  est <- ests[1L]
  
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
        "Install it (install.packages('WeightIt')) or set estimand = 'raw'.",
        call. = FALSE
      )
    }
  } else if (identical(kind, "continuous")) {
    if (!requireNamespace("twangContinuous", quietly = TRUE)) {
      stop(
        "Estimand recovery for continuous exposures requires the 'twangContinuous' package.\n",
        "Install it (install.packages('twangContinuous')) or set estimand = 'raw'.",
        call. = FALSE
      )
    }
    if (!identical(est, "ATE")) {
      stop(
        "For continuous exposures, DAGassist currently supports estimand = 'ATE' only.\n",
        "Set estimand = 'ATE' (or 'raw').",
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
      "\n\nPlease recode it or set estimand = 'raw'.",
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

#labels for weight columns
.dagassist_model_name_labels <- function(estimand) {
  est <- toupper(as.character(estimand))
  switch(
    est,
    ATE  = "(ATE)",
    ATT  = "(ATT)",
    ACDE = "(ACDE)",
    CDE  = "(ACDE)",   # alias
    RAW  = "",
    NONE = "",
    ""
  )
}
