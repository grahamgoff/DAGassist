# R/balance_models.R
#
# exported engine-agnostic balance diagnostics over a user-supplied list of
# fitted models.

##per-model variable extraction 

# variables that drive listwise deletion for a fitted model
.dagassist_model_delete_vars <- function(m, data, extra_vars = NULL) {
  vars <- character(0)
  fml  <- tryCatch(stats::formula(m), error = function(e) NULL)
  if (is.null(fml))
    stop("Could not read a formula from a model. Pass the fitted model itself ",
         "(e.g. the lm/glm/feols/coxph object), not a coeftest()/summary() wrapper.")
  # base + any fixest/felm `| fe | iv | cluster` tail parts
  vars <- .dagassist_spec_vars(fml, data)
  # auto-detect weights / cluster / subset / id from the call
  cl <- tryCatch(stats::getCall(m), error = function(e) NULL)
  if (!is.null(cl)) {
    for (key in c("weights", "cluster", "subset", "id")) {
      if (key %in% names(cl)) {
        ex <- tryCatch(all.vars(cl[[key]]), error = function(e) character(0))
        vars <- c(vars, ex)
      }
    }
  }
  vars <- c(vars, extra_vars)
  intersect(unique(vars), names(data))
}

#covariates to assess for a fitted model 
#RHS vars minus the outcome/LHS
.dagassist_model_covars <- function(m, data, include_outcome = FALSE) {
  fml <- tryCatch(stats::formula(m), error = function(e) NULL)
  if (is.null(fml)) return(character(0))
  sp  <- .strip_fixest_parts(fml)
  v   <- tryCatch(all.vars(sp$base), error = function(e) character(0))
  if (!include_outcome) {
    lhs <- tryCatch(all.vars(sp$base[[2]]), error = function(e) character(0))
    v   <- setdiff(v, lhs)
  }
  intersect(unique(v), names(data))
}

##exported bit

#' Balance diagnostics across a list of fitted models
#'
#' Compares the complete-case analytic sample of a reference model against each
#' other model in a named list, flagging covariates whose sample composition
#' shifts (`|(S)MD| > threshold`). Engine-agnostic: it reads variable names from
#' each model and recomputes samples on `data`, so it works for `lm`, `glm`,
#' `feols`, `coxph`, weighted fits, etc. Models are never refit.
#'
#' @param models Named list of fitted models. The names label the comparisons.
#' @param data The shared source data frame (a superset of every model's rows).
#' @param reference Name or index of the model that defines the baseline sample.
#' @param covariates Optional character vector of covariates to assess. Default:
#'   the union of all models' predictors present in `data`.
#' @param threshold Flag covariates with `|(S)MD|` above this (default 0.1).
#' @param extra_vars Additional variables that drive listwise deletion (e.g. a
#'   weight or cluster column not detected from the call).
#' @param include_outcome Assess the outcome variable too (default FALSE).
#'
#' @return An object of class `DAGassist_balance`: a list with `$reference`,
#'   `$comparisons` (per-comparison (S)MD tables) and `$summary` (one row per
#'   comparison: n_ref, n_cmp, n_covariates, n_flagged, pct_flagged, any_flagged).
#' @export
balance_models <- function(models, data, reference = 1, covariates = NULL,
                           threshold = 0.1, extra_vars = NULL,
                           include_outcome = FALSE) {
  stopifnot(is.list(models), length(models) >= 2L, is.data.frame(data))
  if (is.null(names(models)) || any(!nzchar(names(models))))
    names(models) <- paste0("Model ", seq_along(models))
  
  ref_idx <- if (is.character(reference)) match(reference, names(models)) else as.integer(reference)
  if (is.na(ref_idx) || ref_idx < 1L || ref_idx > length(models))
    stop("`reference` must be a valid model name or index.")
  ref_name <- names(models)[ref_idx]
  
  del_vars <- lapply(models, .dagassist_model_delete_vars, data = data, extra_vars = extra_vars)
  cov_sets <- lapply(models, .dagassist_model_covars, data = data, include_outcome = include_outcome)
  rows     <- lapply(del_vars, function(v) .dagassist_complete_rows(data, v))
  rows_ref <- rows[[ref_idx]]
  
  if (is.null(covariates)) covariates <- Reduce(union, cov_sets)
  covariates <- intersect(covariates, names(data))
  
  comparisons <- list(); summ <- list()
  for (i in seq_along(models)) {
    if (i == ref_idx) next
    nm <- names(models)[i]
    bt <- .dagassist_balance_compare(data, rows_ref, rows[[i]], covariates, threshold)
    comparisons[[nm]] <- bt
    n_assess <- if (is.null(bt)) 0L else nrow(bt)
    n_flag   <- if (is.null(bt)) 0L else sum(bt$flagged)
    summ[[nm]] <- data.frame(
      comparison   = nm,
      n_ref        = sum(rows_ref),
      n_cmp        = sum(rows[[i]]),
      n_covariates = n_assess,
      n_flagged    = n_flag,
      pct_flagged  = if (n_assess) 100 * n_flag / n_assess else NA_real_,
      any_flagged  = n_flag > 0L,
      row.names = NULL, stringsAsFactors = FALSE
    )
  }
  
  out <- list(reference = ref_name, comparisons = comparisons,
              summary = do.call(rbind, summ), threshold = threshold)
  class(out) <- "DAGassist_balance"
  out
}

#' @export
print.DAGassist_balance <- function(x, ...) {
  cat(sprintf("Balance diagnostics (reference: %s)\n", x$reference))
  cat(sprintf("  |(S)MD| > %.2f flags a covariate whose sample shifts vs the reference ",
              x$threshold))
  cat("(binary vars use a raw difference).\n")
  for (nm in names(x$comparisons)) {
    bt <- x$comparisons[[nm]]
    s  <- x$summary[x$summary$comparison == nm, ]
    head <- sprintf("  %s vs %s: n = %d vs %d", x$reference, nm, s$n_ref, s$n_cmp)
    if (is.null(bt) || !nrow(bt)) { cat(head, " (no assessable covariates)\n", sep = ""); next }
    flagged <- bt[bt$flagged, , drop = FALSE]
    if (!nrow(flagged)) { cat(head, "  balanced\n", sep = ""); next }
    cat(head, sprintf("  [!] %d covariate(s) imbalanced\n", nrow(flagged)), sep = "")
    flagged <- flagged[order(-abs(flagged$smd)), , drop = FALSE]
    for (j in seq_len(nrow(flagged))) {
      tag <- if (flagged$type[j] == "binary") "  [raw diff]" else ""
      val <- if (is.finite(flagged$smd[j])) sprintf("% .3f", flagged$smd[j]) else "    Inf"
      cat(sprintf("      %-24s (S)MD = %s%s\n", flagged$variable[j], val, tag))
    }
  }
  invisible(x)
}