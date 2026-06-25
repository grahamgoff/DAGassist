# R/balance.R
#
# Automated balance diagnostics.
# Tests whether the complete-case analytic sample under the author's ORIGINAL
# specification is fundamentally the same as the sample under the minimal,
# canonical, or all-DAG-consistent specifications. For each covariate we compute
# a (standardized) mean difference between the two samples and flag any covariate
# with |(S)MD| > threshold. Mirrors the ESS/weight-diagnostics warning style:
# console-only, gated on `verbose`, printed below the model-comparison table.
#
# (S)MD rules (per professor):
#   - binary variables (0/1, logical, 2-level factor): raw difference in means
#   - continuous / numeric-categorical variables:      standardized,
#         (m_ref - m_cmp) / sqrt((var_ref + var_cmp) / 2)
#   - true unordered factors with >2 levels: expanded to per-level indicator
#         dummies, each treated as binary (raw difference) -- a factor has no
#         meaningful single numeric mean to standardize.

# ---- variable type ----------------------------------------------------------
.dagassist_balance_kind <- function(v) {
  v <- v[!is.na(v)]
  if (!length(v)) return("empty")
  if (is.logical(v)) return("binary")
  if (is.character(v)) v <- factor(v)
  if (is.factor(v)) {
    if (nlevels(droplevels(v)) <= 2L) return("binary")
    return("factor")
  }
  if (is.numeric(v) || inherits(v, "integer")) {
    u <- unique(v)
    if (length(u) <= 2L && all(u %in% c(0, 1))) return("binary")
    return("continuous")
  }
  "unsupported"
}

# coerce a binary-ish column to a 0/1 indicator
.dagassist_to_binary <- function(col) {
  if (is.logical(col)) return(as.numeric(col))
  if (is.numeric(col) || inherits(col, "integer")) {
    u <- sort(unique(col[!is.na(col)]))
    if (length(u) <= 2L && all(u %in% c(0, 1))) return(as.numeric(col))
    return(as.numeric(col == max(u)))      # 2-level non-0/1 numeric
  }
  f <- factor(col)
  as.numeric(f == levels(f)[nlevels(f)])    # indicator of the last level
}

# ---- single (standardized) mean difference ---------------------------------
.dagassist_smd <- function(x_ref, x_cmp, binary = FALSE) {
  x_ref <- x_ref[is.finite(x_ref)]
  x_cmp <- x_cmp[is.finite(x_cmp)]
  if (length(x_ref) < 1L || length(x_cmp) < 1L) return(NA_real_)
  m1 <- mean(x_ref); m2 <- mean(x_cmp)
  if (isTRUE(binary)) return(m1 - m2)                     # raw diff in proportions
  if (length(x_ref) < 2L || length(x_cmp) < 2L) return(NA_real_)
  v1 <- stats::var(x_ref); v2 <- stats::var(x_cmp)
  denom <- sqrt((v1 + v2) / 2)                            # pooled SD
  if (!is.finite(denom) || denom == 0) {
    if (isTRUE(all.equal(m1, m2))) return(0)
    return(Inf * sign(m1 - m2))                          # constant but mismatched
  }
  (m1 - m2) / denom
}

# ---- variables that drive listwise deletion for a spec (incl. FE/cluster) ---
.dagassist_spec_vars <- function(formula, data, exp_nm = NULL, out_nm = NULL,
                                 engine_args = list()) {
  sp <- .strip_fixest_parts(formula)
  vars <- unique(c(all.vars(sp$base), exp_nm, out_nm))
  # fixest / IV / FE tail segments (each top-level '|' piece)
  s_full <- paste(deparse(formula, width.cutoff = 500L), collapse = " ")
  parts  <- .split_top_level(s_full, sep = "|")
  if (length(parts) >= 2L) {
    for (tt in trimws(parts[-1])) {
      if (!nzchar(tt)) next
      f_tt <- tryCatch(stats::as.formula(paste("~", tt), env = environment(formula)),
                       error = function(e) NULL)
      if (!is.null(f_tt)) vars <- c(vars, all.vars(f_tt))
    }
  }
  # cluster vars carried in engine_args
  for (key in c("cluster", "clusters")) {
    if (key %in% names(engine_args)) {
      cl <- engine_args[[key]]
      if (inherits(cl, "formula")) vars <- c(vars, all.vars(cl))
      else if (is.character(cl) && length(cl) == 1L) vars <- c(vars, cl)
    }
  }
  intersect(unique(vars), names(data))
}

.dagassist_complete_rows <- function(data, vars) {
  if (!length(vars)) return(rep(TRUE, nrow(data)))
  stats::complete.cases(data[, vars, drop = FALSE])
}

# ---- per-covariate balance for one comparison ------------------------------
.dagassist_balance_compare <- function(data, rows_ref, rows_cmp, covars,
                                       threshold = 0.1) {
  results <- list()
  add <- function(key, label, type, smd) {
    results[[key]] <<- list(variable = label, type = type, smd = smd)
  }
  for (v in covars) {
    col  <- data[[v]]
    kind <- .dagassist_balance_kind(col)
    if (kind %in% c("empty", "unsupported")) next
    if (kind == "factor") {
      f <- factor(col)
      for (lv in levels(f)) {
        ind <- as.numeric(f == lv)
        add(paste0(v, ":", lv), paste0(v, " [", lv, "]"), "binary",
            .dagassist_smd(ind[rows_ref], ind[rows_cmp], binary = TRUE))
      }
    } else if (kind == "binary") {
      ind <- .dagassist_to_binary(col)
      add(v, v, "binary",
          .dagassist_smd(ind[rows_ref], ind[rows_cmp], binary = TRUE))
    } else {
      x <- as.numeric(col)
      add(v, v, "continuous",
          .dagassist_smd(x[rows_ref], x[rows_cmp], binary = FALSE))
    }
  }
  if (!length(results)) return(NULL)
  df <- data.frame(
    variable = vapply(results, function(z) z$variable, character(1)),
    type     = vapply(results, function(z) z$type,     character(1)),
    smd      = vapply(results, function(z) z$smd,      numeric(1)),
    row.names = NULL, stringsAsFactors = FALSE
  )
  df$flagged <- (is.finite(df$smd) & abs(df$smd) > threshold) | is.infinite(df$smd)
  df
}

# ---- printer (mirrors .dagassist_print_weight_diagnostics) -----------------
.dagassist_print_balance_diagnostics <- function(x, threshold = 0.1,
                                                 include_outcome = FALSE) {
  data <- x$.__data
  if (is.null(data) || !is.data.frame(data) || !nrow(data)) return(invisible(NULL))
  fmls <- x$formulas
  if (is.null(fmls) || is.null(fmls$original)) return(invisible(NULL))
  
  exp_nm <- tryCatch(get_by_role(x$roles, "exposure"), error = function(e) NA_character_)
  out_nm <- tryCatch(get_by_role(x$roles, "outcome"),  error = function(e) NA_character_)
  engine_args <- x$settings$engine_args
  if (!is.list(engine_args)) engine_args <- list()
  
  # comparison specs (besides Original)
  specs <- list()
  if (length(fmls$minimal_list)) {
    for (i in seq_along(fmls$minimal_list))
      specs[[sprintf("Minimal %d", i)]] <- fmls$minimal_list[[i]]
  } else if (!is.null(fmls$minimal)) {
    specs[["Minimal 1"]] <- fmls$minimal
  }
  if (!is.null(fmls$canonical)) specs[["Canonical"]] <- fmls$canonical
  if (!is.null(fmls$canonical_excl) && is.list(fmls$canonical_excl)) {
    for (nm in names(fmls$canonical_excl)) {
      lbl <- switch(nm, nct = "Canon. (-NCT)", nco = "Canon. (-NCO)",
                    paste0("Canonical (", nm, ")"))
      specs[[lbl]] <- fmls$canonical_excl[[nm]]
    }
  }
  if (!length(specs)) return(invisible(NULL))
  
  covars_of <- function(fml) {
    sp <- .strip_fixest_parts(fml)
    v  <- tryCatch(all.vars(sp$base), error = function(e) character(0))
    if (!include_outcome && !is.na(out_nm)) v <- setdiff(v, out_nm)
    intersect(unique(v), names(data))
  }
  rows_orig <- .dagassist_complete_rows(
    data, .dagassist_spec_vars(fmls$original, data, exp_nm, out_nm, engine_args))
  
  cat("\nBalance diagnostics:\n")
  cat("  legend: (S)MD compares covariate means between the Original complete-case sample\n")
  cat(sprintf("          and each spec's sample; |(S)MD| > %.2f flags a covariate whose sample\n", threshold))
  cat("          composition shifts (binary vars use a raw difference in means).\n")
  
  emit_one <- function(ref_label, rows_ref, fml_ref, cmp_label, rows_cmp, fml_cmp) {
    covars <- union(covars_of(fml_ref), covars_of(fml_cmp))
    n_ref <- sum(rows_ref); n_cmp <- sum(rows_cmp)
    head  <- sprintf("  %s vs %s: n = %d vs %d", ref_label, cmp_label, n_ref, n_cmp)
    bt <- .dagassist_balance_compare(data, rows_ref, rows_cmp, covars, threshold)
    if (is.null(bt) || !nrow(bt)) { cat(head, " (no assessable covariates)\n", sep = ""); return(invisible()) }
    flagged <- bt[bt$flagged, , drop = FALSE]
    if (!nrow(flagged)) {
      cat(head, clr_green("  balanced"), "\n", sep = "")
    } else {
      cat(head, clr_red(sprintf("  [!] %d covariate(s) imbalanced", nrow(flagged))), "\n", sep = "")
      flagged <- flagged[order(-abs(flagged$smd)), , drop = FALSE]
      for (i in seq_len(nrow(flagged))) {
        tag <- if (flagged$type[i] == "binary") "  [raw diff]" else ""
        val <- if (is.finite(flagged$smd[i])) sprintf("% .3f", flagged$smd[i]) else "    Inf"
        cat(clr_yellow(sprintf("      %-24s (S)MD = %s%s\n", flagged$variable[i], val, tag)))
      }
    }
  }
  
  # (1)-(3): Original vs each spec
  for (nm in names(specs)) {
    rows_cmp <- .dagassist_complete_rows(
      data, .dagassist_spec_vars(specs[[nm]], data, exp_nm, out_nm, engine_args))
    emit_one("Original", rows_orig, fmls$original, nm, rows_cmp, specs[[nm]])
  }
  
  # (4): Minimal 1 vs Canonical (only if both exist and differ)
  min1 <- if (length(fmls$minimal_list)) fmls$minimal_list[[1]] else fmls$minimal
  if (!is.null(min1) && !is.null(fmls$canonical) && !.same_formula(min1, fmls$canonical)) {
    rows_min <- .dagassist_complete_rows(
      data, .dagassist_spec_vars(min1, data, exp_nm, out_nm, engine_args))
    rows_can <- .dagassist_complete_rows(
      data, .dagassist_spec_vars(fmls$canonical, data, exp_nm, out_nm, engine_args))
    emit_one("Minimal 1", rows_min, min1, "Canonical", rows_can, fmls$canonical)
  }
  invisible(NULL)
}