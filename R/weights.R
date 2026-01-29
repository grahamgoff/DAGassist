# weights.R
#
# Estimand recovery helpers

.dagassist_exposure_kind <- function(Tvar,
                                     max_categorical_unique = 10L,
                                     integer_tol = sqrt(.Machine$double.eps)) {
  if (is.null(Tvar)) return("unsupported")
  
  v <- stats::na.omit(Tvar)
  if (!length(v)) return("unsupported")
  
  # logical -> binary
  if (is.logical(v)) return("binary")
  
  # character -> categorical
  if (is.character(v)) v <- factor(v)
  
  # factor -> binary vs categorical
  if (is.factor(v)) {
    k <- nlevels(v)
    if (k == 2L) return("binary")
    if (k >= 3L) return("categorical")
    return("unsupported")
  }
  
  # numeric/integer -> binary vs categorical vs continuous
  if (is.numeric(v) || inherits(v, "integer")) {
    u <- sort(unique(v))
    k <- length(u)
    
    if (k == 2L && all(u %in% c(0, 1))) return("binary")
    
    # "coded categories" heuristic: small unique & integer-like values
    integer_like <- all(abs(u - round(u)) < integer_tol)
    if (k >= 3L && k <= max_categorical_unique && integer_like) {
      return("categorical")
    }
    
    if (k >= 3L) return("continuous")
  }
  
  "unsupported"
}

.dagassist_normalize_weights_args <- function(args) {
  if (is.null(args)) return(list())
  if (!is.list(args)) stop("`weights_args` must be a list.", call. = FALSE)

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
  is_weighted <- grepl("\\((SATE|SATT)\\)\\s*$", model_name, ignore.case = TRUE)
  is_acde <- grepl("\\((SACDE|SCDE)\\)\\s*$", model_name, ignore.case = TRUE)
  
  base_name <- sub("\\s*\\((SATE|SATT|SACDE|SCDE)\\)\\s*$", "", model_name, ignore.case = TRUE)
  
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
                   choices = c("RAW","NONE","SATE","SATT","SACDE","SCDE"),
                   several.ok = TRUE)
  est[est == "NONE"] <- "RAW"
  est[est == "SCDE"]  <- "SACDE"
  unique(est)
}

# ---- Normalize ACDE spec list ----
.dagassist_normalize_acde_spec <- function(acde) {
  if (is.null(acde)) acde <- list()
  if (!is.list(acde)) stop("`sacde` must be a list.", call. = FALSE)
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
  ests <- unique(.dagassist_normalize_estimand(estimand))
  
  # ACDE/CDE requires at least one mediator in the DAG / formula
  wants_acde <- any(ests %in% c("SACDE", "SCDE"))
  if (isTRUE(wants_acde)) {
    has_med <- FALSE
    if (!is.null(roles)) {
      if ("role" %in% names(roles)) {
        has_med <- any(roles$role == "mediator")
      }
      if (!isTRUE(has_med) && "is_mediator" %in% names(roles)) {
        has_med <- any(isTRUE(roles$is_mediator))
      }
    }
    if (!isTRUE(has_med)) {
      stop(
        paste0(
          "You requested estimand = 'SACDE' (alias: 'SCDE'), but no mediator node(s) were detected in your DAG ",
          "for this exposure/outcome pair.\n",
          "SACDE/SCDE is only defined when at least one mediator exists.\n\n",
          "Fix options:\n",
          "  1) Use estimand = 'SATE'/'SATT' for total effects (when no mediators are present), OR\n",
          "  2) Use estimand = 'RAW' to report the naive regression output.\n"
        ),
        call. = FALSE
      )
    }
  }
  # allow ATE/ATT if formula includes mediators; will omit automatically
  if (!isTRUE(auto_acde)) return(estimand)
  
  wants_total <- any(ests %in% c("SATE", "SATT"))
  if (!isTRUE(wants_total)) return(estimand)
  
  controls_mediator <- .dagassist_formula_controls_mediator(
    formula,
    roles,
    include_descendants = include_descendants
  )
  if (!isTRUE(controls_mediator)) return(estimand)
  
  estimand
}

.dagassist_safe_descendants <- function(dag, node) {
  tryCatch(dagitty::descendants(dag, node), error = function(e) character(0))
}

.dagassist_safe_ancestors <- function(dag, node) {
  tryCatch(dagitty::ancestors(dag, node), error = function(e) character(0))
}

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
    stop("SACDE requires storing the evaluated DAG on the report as `x$dag`.", call. = FALSE)
  }
  dag <- x$dag
  exp_nm <- get_by_role(x$roles, "exposure")
  out_nm <- get_by_role(x$roles, "outcome")
  
  # mediators
  m_terms <- .dagassist_infer_acde_mediators(x, acde)
  if (!length(m_terms)) {
    stop("SACDE requested, but no mediator(s) could be inferred. Provide sacde = list(m = c('M1','M2')).",
         call. = FALSE)
  }
  
  # strip fixest/random-effects tails from base formula (sequential_g uses | separators)
  sp <- .strip_fixest_parts(base_fml)
  base <- sp$base
  
  rhs_terms <- .rhs_terms_safe(base)  # term labels (includes exposure if present)
  rhs_terms <- unique(rhs_terms)
  ## sequential g is structured as (original|intermediate confounders|mediators)
  ## here, i initially coded all mediators as coming in the third category,
  ## which always led to an empty intermediate confounders set
  ## instead, need to differentiate simple mediators from mediators that are ancestors
  ## of other mediators. the latter will go in the middle bucket as intermediate confounders
  # rhs_terms is for the current base formula (Original/Minimal/Canonical).
  # mediators must come from the original model, or min/can will auto-drop and crash
  sp_orig <- .strip_fixest_parts(x$formulas$original)
  rhs_terms_orig <- unique(.rhs_terms_safe(sp_orig$base))
  
  m_terms <- intersect(m_terms, rhs_terms_orig)
  
  descA <- .dagassist_safe_descendants(dag, exp_nm)
  ancY  <- .dagassist_safe_ancestors(dag, out_nm)
  
  #  compute ancM using the final mediator set
  ancM <- unique(unlist(lapply(m_terms, function(m) .dagassist_safe_ancestors(dag, m))))
  
  # define intermediate confounders according to Acharya, Blackwell and Sen (2016)
  # Z are post-treatment covariates affected by A (treatment) that affect both M and Y:
  #  i.e., Z in Desc(A) intersect Anc(M) intersect Anc(Y)
  # and  exclude A, Y, and the mediator(s) themselves
  nodes <- names(dag)
  
  cand <- setdiff(nodes, c(exp_nm, out_nm, m_terms))
  
  z_auto <- intersect(cand, descA)
  z_auto <- intersect(z_auto, ancY)
  z_auto <- intersect(z_auto, ancM)
  
  # drop post-mediator nodes
  descM <- unique(unlist(lapply(m_terms, function(m) .dagassist_safe_descendants(dag, m))))
  z_auto <- setdiff(z_auto, descM)
  
  z_auto <- unique(z_auto)
  
  # user override
  if (!is.null(acde$z)) z_terms <- unique(as.character(acde$z)) else z_terms <- z_auto
  #force exclude exposure from z
  z_terms <- setdiff(z_terms, exp_nm)
  # X: everything else (excluding exposure, mediators, Z)
  x_auto <- setdiff(rhs_terms, c(exp_nm, m_terms, z_terms))
  if (!is.null(acde$x)) x_terms <- unique(as.character(acde$x)) else x_terms <- x_auto
  #only validate term existence against data if DAGassist has a data.frame stored.
  #in most cases, this will not be used because most users will just include
  #data arg inside regression call
  data_names <- NULL
  if (!is.null(x$.__data) && is.data.frame(x$.__data)) {
    data_names <- names(x$.__data)
    # Drop non-data term strings 
    x_terms <- .dagassist_terms_must_use_data(x_terms, data_names)
    z_terms <- .dagassist_terms_must_use_data(z_terms, data_names)
    m_terms <- .dagassist_terms_must_use_data(m_terms, data_names)
  }
  # if no mediators at this point, helpful error code
  if (!length(m_terms)) {
    stop(
      "SACDE requested, but mediator terms are empty after internal filtering. ",
      "This usually happens when DAGassist was called without `data=` (so `x$.__data` is NULL). ",
      "Either (i) call DAGassist(..., data = <your data.frame>), or (ii) provide `acde = list(m = ...)`.",
      call. = FALSE
    )
  }
  
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
  ##dropped_fe must exist even if no FE or it breaks
  dropped_fe <- character(0)
  if (length(fe_vars)) {
    dx <- .dagassist_drop_terms_collinear_with_fe(x_terms, x$.__data, fe_vars)
    x_terms <- dx$keep
    
    dz <- .dagassist_drop_terms_collinear_with_fe(z_terms, x$.__data, fe_vars)
    z_terms <- dz$keep
    
    # collect for printer method
    dropped_fe <- unique(c(dx$dropped, dz$dropped))
  }
  # Build text formula
  # First block always includes exposure; append X if any
  block1 <- paste(c(exp_nm, x_terms), collapse = " + ")
  if (!nzchar(block1)) block1 <- exp_nm
  ##set to 0 instead of 1 because 1 trips singularity in first stage, causing summary() to fail
  blockZ <- if (length(z_terms)) paste(z_terms, collapse = " + ") else "0"
  blockM <- paste(m_terms, collapse = " + ")
  
  f_txt <- paste0(out_nm, " ~ ", block1, " | ", blockZ, " | ", blockM)
  #cache for printer later
  f_acde <- stats::as.formula(f_txt, env = environment(base_fml))
  attr(f_acde, "dagassist_fe_collinear_dropped") <- dropped_fe
  # store mediator terms for guardrails 
  attr(f_acde, "dagassist_acde_m_terms") <- m_terms
  f_acde
}

# ---- Add ACDE models (sequential g-estimation) ----
.dagassist_add_acde_models <- function(x, mods) {
  if (!requireNamespace("DirectEffects", quietly = TRUE)) {
    stop(
      "Estimand recovery for SACDE/SCDE requires the 'DirectEffects' package.\n",
      "Install it (install.packages('DirectEffects')) or set estimand = 'raw'.",
      call. = FALSE
    )
  }
  
  data <- x$.__data
  if (is.null(data)) {
    stop(
      "Original data not found on the report object.\n",
      "SACDE requires calling DAGassist() with the `data` argument.",
      call. = FALSE
    )
  }
  
  acde <- .dagassist_normalize_acde_spec(x$settings$acde)
  
  # args for sequential_g (user-supplied)
  de_args <- x$settings$directeffects_args
  if (is.null(de_args)) de_args <- list()
  
  nms <- names(mods)
  if (!length(nms)) return(mods)
  
  # helpers
  base_of <- function(nm) sub("\\s*\\((SATE|SATT|SACDE|SCDE)\\)\\s*$", "", nm, ignore.case = TRUE)
  is_acde <- function(nm) grepl("\\((SACDE|SCDE)\\)\\s*$", nm, ignore.case = TRUE)
  is_weighted <- function(nm) grepl("\\((SATE|SATT)\\)\\s*$", nm, ignore.case = TRUE)
  
  # --- 1) Fit ACDE for each BASE spec (Original / Minimal k / Canonical / etc.) ---
  base_specs <- unique(base_of(nms[!is_acde(nms)]))
  # keep only bases that actually exist as a base column
  base_specs <- base_specs[base_specs %in% nms]
  
  acde_fits <- list()
  
  for (b in base_specs) {
    # don't refit if it's already present
    acde_name <- paste0(b, " ", .dagassist_model_name_labels("SACDE"))
    if (acde_name %in% nms) next
    
    base_fml <- .dagassist_formula_for_model_name(x, b)
    if (is.null(base_fml)) next
    
    f_acde <- .dagassist_build_acde_formula(base_fml, x, acde)
    
    # guardrail for categorical mediators
    m_terms <- attr(f_acde, "dagassist_acde_m_terms", exact = TRUE)
    guard_msg <- .dagassist_acde_guard_mediators(data, m_terms)
    
    if (!is.null(guard_msg)) {
      fit <- structure(
        list(error = guard_msg, formula = f_acde),
        class = "DAGassist_fit_error"
      )
    } else {
      fit <- .safe_fit(DirectEffects::sequential_g, f_acde, data, de_args)
    }
    
    # attach metadata for console printing later
    if (!inherits(fit, "DAGassist_fit_error")) {
      attr(fit, "dagassist_estimand") <- "SACDE"
      attr(fit, "dagassist_acde_spec") <- b
      attr(fit, "dagassist_acde_formula") <- f_acde
      attr(fit, "dagassist_fe_collinear_dropped") <- attr(
        f_acde, "dagassist_fe_collinear_dropped", exact = TRUE
      )
    }
    
    acde_fits[[b]] <- fit
  }
  
  if (!length(acde_fits)) return(mods)
  
  # --- 2) Determine where to insert each ACDE column: after the last column
  #         belonging to that base spec (raw + any ATE/ATT columns) ---
  insert_after <- integer(0)
  for (b in names(acde_fits)) {
    idxs <- which(base_of(nms) == b & !is_acde(nms))
    if (!length(idxs)) next
    insert_after[b] <- max(idxs)
  }
  
  # --- 3) Rebuild list with ACDE interleaved ---
  out <- list()
  for (i in seq_along(nms)) {
    nm <- nms[[i]]
    out[[nm]] <- mods[[nm]]
    
    b <- base_of(nm)
    if (!is.na(insert_after[b]) && i == insert_after[b] && !is.null(acde_fits[[b]])) {
      acde_name <- paste0(b, " ", .dagassist_model_name_labels("SACDE"))
      out[[acde_name]] <- acde_fits[[b]]
    }
  }
  
  out
}

# add whichever estimands are requested
.dagassist_add_estimand_models <- function(x, mods) {
  ests <- .dagassist_normalize_estimand(x$settings$estimand)
  if (!length(ests) || identical(ests, "RAW")) return(mods)
  
  out <- mods
  # weights first (SATE/SATT)
  if ("SATE" %in% ests) out <- .dagassist_add_weighted_models(x, out, estimand = "SATE")
  if ("SATT" %in% ests) out <- .dagassist_add_weighted_models(x, out, estimand = "SATT")
  
  # ACDE last
  if ("SACDE" %in% ests) out <- .dagassist_add_acde_models(x, out)
  
  out
}


# Add weighted versions of each model column (ATE/ATT) using WeightIt
# Key fix: compute weights *per model spec* (Minimal k vs Canonical, etc.)
# and do NOT create "Original (ATE)" or "Original (ATT)" columns.
.dagassist_add_weighted_models <- function(x, mods, estimand = NULL) {
  
  ests <- .dagassist_normalize_estimand(
    if (!is.null(estimand)) estimand else x$settings$estimand
  )
  
  # Weighting only applies to total-effect estimands
  ests <- intersect(ests, c("SATE", "SATT"))
  if (!length(ests)) return(mods)
  est <- ests[1L]
  
  data0 <- x$.__data
  if (is.null(data0)) {
    stop(
      "Original data not found on the report object.\n",
      "Estimand recovery requires calling DAGassist() with the `data` argument.",
      call. = FALSE
    )
  }
  
  exp_nm <- get_by_role(x$roles, "exposure")
  out_nm <- get_by_role(x$roles, "outcome")
  
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
  
  if (!exp_nm %in% names(data0)) {
    stop(
      "Exposure variable '", exp_nm, "' was identified in the DAG but not found in `data`.\n",
      "Please check that the DAG node names and data column names match.",
      call. = FALSE
    )
  }
  
  # Weight args: user-configurable, but filtered to WeightIt::weightit() formals
  wargs <- .dagassist_normalize_weights_args(x$settings$weights_args)
  trim_at <- wargs[["trim_at"]]          # DAGassist-specific (optional)
  wargs[["trim_at"]] <- NULL
  
  ##dependency checks for the ATE dependencies
  if (!requireNamespace("WeightIt", quietly = TRUE)) {
    stop(
      "Estimand recovery requires the 'WeightIt' package.\n",
      "Install it (install.packages('WeightIt')) or set estimand = 'raw'.",
      call. = FALSE
    )
  }
  
  if (!requireNamespace("marginaleffects", quietly = TRUE)) {
    stop(
      "Estimand recovery requires the 'marginaleffects' package.\n",
      "Install it (install.packages('marginaleffects')) or set estimand = 'raw'.",
      call. = FALSE
    )
  }
  
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
  
  # --- helper: compute weights and fit one weighted spec on complete-case data ---
  .fit_weighted_one <- function(model_name) {
    
    # Do NOT produce "Original (ATE)/(ATT)" columns
    if (identical(model_name, "Original")) return(NULL)
    
    fml <- .dagassist_formula_for_model_name(x, model_name)
    if (is.null(fml)) return(NULL)
    
    # Controls for the treatment model come from *this model's* RHS (minus X and Y)
    rhs <- .rhs_terms_safe(fml)
    controls <- setdiff(rhs, c(exp_nm, out_nm))
    controls <- intersect(controls, names(data0))
    controls <- unique(controls)
    
    # Build treatment formula: X ~ controls (or ~1 if none)
    if (length(controls)) {
      f_treat <- stats::as.formula(paste(exp_nm, "~", paste(controls, collapse = " + ")))
    } else {
      f_treat <- stats::as.formula(paste(exp_nm, "~ 1"))
    }
    
    # Build complete-case analytic data for THIS spec.
    # Use variables needed for treatment + outcome model evaluation.
    base_fml <- .strip_fixest_parts(fml)$base
    vars_need <- unique(c(all.vars(base_fml), exp_nm, out_nm, controls))
    vars_need <- intersect(vars_need, names(data0))
    
    data_cc <- stats::na.omit(data0[, vars_need, drop = FALSE])
    if (!nrow(data_cc)) return(NULL)
    
    # Determine exposure kind on this model's analytic sample
    kind <- .dagassist_exposure_kind(data_cc[[exp_nm]])
    if (!kind %in% c("binary", "categorical", "continuous")) {
      u <- try(sort(unique(stats::na.omit(data_cc[[exp_nm]]))), silent = TRUE)
      stop(
        "Estimand recovery supports:\n",
        "  * Binary exposures (0/1 numeric, logical, or 2-level factor)\n",
        "  * Categorical exposures (factor/character or small-unique integer-like codes)\n",
        "  * Continuous numeric exposures\n\n",
        "Exposure '", exp_nm, "' is class: ", paste(class(data_cc[[exp_nm]]), collapse = "/"),
        if (!inherits(u, "try-error")) paste0("\nObserved values (unique): ", paste(u, collapse = ", ")) else "",
        "\n\nPlease recode it or set estimand = 'raw'.",
        call. = FALSE
      )
    }
    
    # WeightIt sometimes behaves better with ordered factors for categorical-coded numerics
    data_wt <- data_cc
    if (identical(kind, "categorical") && !is.factor(data_wt[[exp_nm]])) {
      data_wt[[exp_nm]] <- factor(data_wt[[exp_nm]], ordered = TRUE)
    }
    
    # Filter args -> only those accepted by WeightIt::weightit()
    fa <- .dagassist_filter_args(wargs, WeightIt::weightit)
    if (length(fa$drop)) {
      warning(
        "Ignoring these weights_args for WeightIt::weightit(): ",
        paste(fa$drop, collapse = ", "),
        call. = FALSE
      )
    }
    
    # run weightit, but keep its warnings out of the dagassist console to 
    # reduce clutter and confusion
    wtobj <- suppressWarnings(
      do.call(
        WeightIt::weightit,
        c(
          list(
            formula  = f_treat,
            data     = data_wt,
            method   = "glm",
            estimand = est
          ),
          fa$keep
        )
      )
    )
    
    w <- wtobj$weights
    
    # Optional trimming/capping
    if (!is.null(trim_at)) {
      if (!is.numeric(trim_at) || length(trim_at) != 1L || trim_at <= 0 || trim_at >= 1) {
        stop("weights_args$trim_at must be a single number in (0, 1).", call. = FALSE)
      }
      if ("trim" %in% getNamespaceExports("WeightIt")) {
        w <- WeightIt::trim(w, at = trim_at)
      } else {
        cap <- as.numeric(stats::quantile(w, probs = trim_at, na.rm = TRUE, names = FALSE))
        w <- pmin(w, cap)
      }
    }
    
    if (length(w) != nrow(data_cc)) {
      stop(
        "WeightIt returned ", length(w), " weights for data with ",
        nrow(data_cc), " rows.\n",
        "This indicates a mismatch between the analytic sample and WeightIt's internal sample.\n",
        "Inspect the treatment model and missingness in controls.",
        call. = FALSE
      )
    }
    
    # Fit weighted version of THIS model on THIS model’s CC data
    engine_args_w <- utils::modifyList(engine_args, list(weights = w))
    #specifically suppress binomial warning, which won't be caught in the prior
    #warning suppression. it may be classed as a message or something.
    fit_w <- withCallingHandlers(
      .safe_fit(engine, fml, data_cc, engine_args_w),
      warning = function(wrn) {
        msg <- conditionMessage(wrn)
        if (grepl("non-integer #successes in a binomial glm", msg, fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
    
    # IMPORTANT: for the model comparison table, store marginaleffects output
    # (response-scale average comparisons) rather than the weighted log-odds model.
    me <- tryCatch(
      marginaleffects::avg_comparisons(
        fit_w,
        type = "response",
        wts  = w
      ),
      error = function(e) fit_w
    )
    
    if (!inherits(me, "DAGassist_fit_error")) {
      # If exposure is multi-level, avg_comparisons can return multiple contrasts.
      # Keep only the first exposure contrast so the comparison table stays stable.
      if ("term" %in% names(me) && any(me$term == exp_nm)) {
        rows_exp <- which(me$term == exp_nm)
        if (length(rows_exp) > 1L) {
          me <- me[setdiff(seq_len(nrow(me)), rows_exp[-1L]), , drop = FALSE]
          warning(
            sprintf(
              "Exposure '%s' has >2 levels; keeping only the first contrast in the (%s) column. To obtain all contrasts, call marginaleffects::avg_comparisons(attr(<this column object>, 'dagassist_weighted_fit'), type='response', wts=attr(<this column object>, 'dagassist_weights')).",
              exp_nm, est
            ),
            call. = FALSE
          )
        }
      }
      
      # Preserve metadata for diagnostics/debugging
      attr(me, "dagassist_estimand") <- est
      attr(me, "dagassist_weightit") <- wtobj
      attr(me, "dagassist_treat_formula") <- f_treat
      attr(me, "dagassist_trim_at") <- trim_at
      attr(me, "dagassist_weights") <- w
      attr(me, "dagassist_weighted_fit") <- fit_w
    }
    
    me
  }
  
  # Compute weighted fit per spec
  weighted_mods <- list()
  for (nm in names(mods)) {
    fit_w <- .fit_weighted_one(nm)
    if (!is.null(fit_w)) weighted_mods[[nm]] <- fit_w
  }
  
  # Splice weighted columns in directly after their base column
  est_label <- paste0(" (", est, ")")
  mods_out <- list()
  for (nm in names(mods)) {
    mods_out[[nm]] <- mods[[nm]]
    if (!is.null(weighted_mods[[nm]])) {
      mods_out[[paste0(nm, est_label)]] <- weighted_mods[[nm]]
    }
  }
  
  mods_out
}
#labels for weight columns
.dagassist_model_name_labels <- function(estimand) {
  est <- toupper(as.character(estimand))
  switch(
    est,
    ATE  = "(SATE)",
    ATT  = "(SATT)",
    ACDE = "(SACDE)",
    CDE  = "(SACDE)",   # alias
    RAW  = "",
    NONE = "",
    ""
  )
}

# ---- Guardrail: ACDE mediator types ----
# DirectEffects::sequential_g() is fragile when mediators are not numeric columns.
# This guard identifies non-numeric mediators and returns an actionable message.
.dagassist_acde_guard_mediators <- function(data, m_terms) {
  if (is.null(m_terms) || !length(m_terms)) return(NULL)
  
  m_terms <- unique(as.character(m_terms))
  m_terms <- intersect(m_terms, names(data))
  if (!length(m_terms)) return(NULL)
  
  classes <- vapply(m_terms, function(nm) paste(class(data[[nm]]), collapse = "/"), character(1))
  is_bad  <- vapply(m_terms, function(nm) {
    v <- data[[nm]]
    is.factor(v) || is.character(v) || is.logical(v)
  }, logical(1))
  
  bad <- m_terms[is_bad]
  if (!length(bad)) return(NULL)
  
  # levels (only meaningful for factor/character)
  lvl_txt <- vapply(bad, function(nm) {
    v <- data[[nm]]
    if (is.character(v)) v <- factor(v)
    if (is.factor(v)) {
      lv <- levels(v)
      paste0("levels=", length(lv), if (length(lv) && length(lv) <= 8) paste0(" (", paste(lv, collapse = ", "), ")") else "")
    } else if (is.logical(v)) {
      "logical"
    } else {
      ""
    }
  }, character(1))
  
  bullets <- paste0(
    "  - ", bad, "  [class: ", classes[match(bad, m_terms)], 
    ifelse(nzchar(lvl_txt), paste0("; ", lvl_txt), ""),
    "]"
  )
  
  paste0(
    "SACDE fit aborted before calling DirectEffects::sequential_g().\n\n",
    "Reason:\n",
    "  At least one mediator is non-numeric (factor/character/logical).\n",
    "  DirectEffects::sequential_g() can throw `subscript out of bounds` in this case, ",
    "  because categorical mediators expand to multiple model-matrix columns which do not ",
    "  match the mediator term labels.\n\n",
    "Problematic mediator(s):\n",
    paste(bullets, collapse = "\n"), "\n\n",
    "How to fix:\n",
    "  1) Recode mediator(s) to numeric before calling DAGassist (e.g., binary 0/1).\n",
    "  2) One-hot encode multi-category mediators into numeric dummy columns, then pass\n",
    "     those dummy names explicitly via `acde = list(m = c(\"M1\",\"M2\", ...))`. \n",
    "     Either ensure your DAG nodes match those column names, or use imply = FALSE to prevent mismatch issues. \n",
    "  3) Exclude the categorical mediator(s) from SACDE by explicitly setting `sacde$m`.\n"
  )
}