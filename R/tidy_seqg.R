# tidy_seqg.R
# broom tidier + vcov for DirectEffects::sequential_g output ("seqg")

#' @export
vcov.seqg <- function(object, ...) {
  if (!requireNamespace("DirectEffects", quietly = TRUE)) {
    stop("vcov.seqg() requires the 'DirectEffects' package.", call. = FALSE)
  }
  
  # If DirectEffects ever adds a native vcov method or stores one, use it.
  if (!is.null(object$vcov) && is.matrix(object$vcov)) {
    return(object$vcov)
  }
  
  # sequential_g() returns:
  #  - first_mod: first-stage model
  #  - X: model matrix for second stage
  #  - terms: list of terms objects, incl mediator terms
  # See DirectEffects documentation for details.
  if (is.null(object$first_mod)) stop("seqg object missing `first_mod`.", call. = FALSE)
  if (is.null(object$X))        stop("seqg object missing `X` (second-stage model matrix).", call. = FALSE)
  
  X1 <- tryCatch(stats::model.matrix(object$first_mod), error = function(e) NULL)
  if (is.null(X1)) stop("Could not build X1 via model.matrix(first_mod).", call. = FALSE)
  
  X2 <- object$X
  
  # Mediator term labels: object$terms is typically a list with an entry for mediators.
  med.vars <- NULL
  if (is.list(object$terms) && length(object$terms)) {
    termsM <- NULL
    if (!is.null(object$terms$M)) {
      termsM <- object$terms$M
    } else if (length(object$terms) >= 2L) {
      termsM <- object$terms[[2L]]
    }
    if (!is.null(termsM)) {
      med.vars <- attr(termsM, "term.labels")
    }
  }
  
  # Last resort: try colnames of M if present (mediator model matrix)
  if (is.null(med.vars) && !is.null(object$M)) {
    med.vars <- colnames(object$M)
  }
  if (is.null(med.vars)) med.vars <- character(0)
  
  DirectEffects::seq_g_vcov(
    first_mod = object$first_mod,
    direct_mod = object,
    X1 = X1,
    X2 = X2,
    med.vars = med.vars
  )
}

#' @export
tidy.seqg <- function(x,
                      conf.int = FALSE,
                      conf.level = 0.95,
                      ...) {
  
  co <- tryCatch(stats::coef(x), error = function(e) NULL)
  if (is.null(co)) {
    return(data.frame(
      term = character(0),
      estimate = numeric(0),
      std.error = numeric(0),
      statistic = numeric(0),
      p.value = numeric(0)
    ))
  }
  
  # Compute SEs via vcov.seqg; if that fails, return NA SEs (but do not crash modelsummary).
  V <- tryCatch(stats::vcov(x), error = function(e) NULL)
  se <- rep(NA_real_, length(co))
  names(se) <- names(co)
  
  if (!is.null(V) && is.matrix(V)) {
    d <- sqrt(diag(V))
    # Align by name if possible
    if (!is.null(names(d))) {
      se[names(co)] <- d[names(co)]
    } else {
      se[seq_along(co)] <- d[seq_along(co)]
    }
  }
  
  # ---- DAGassist presentation control ----
  # If DAGassist attached exposure metadata, keep ONLY the exposure term by default.
  meta <- attr(x, "dagassist_meta", exact = TRUE)
  if (is.list(meta) && isTRUE(meta$tidy_only_exposure) && !is.null(meta$exposure)) {
    keep <- meta$exposure
    idx <- which(names(co) %in% keep)
  } else {
    idx <- seq_along(co)
  }
  
  out <- data.frame(
    term      = names(co)[idx],
    estimate  = unname(co[idx]),
    std.error = unname(se[idx]),
    statistic = NA_real_,
    p.value   = NA_real_,
    stringsAsFactors = FALSE
  )
  
  # Optional: Wald z/t if SE exists
  ok <- is.finite(out$std.error) & out$std.error > 0
  out$statistic[ok] <- out$estimate[ok] / out$std.error[ok]
  out$p.value[ok] <- 2 * stats::pnorm(abs(out$statistic[ok]), lower.tail = FALSE)
  
  out
}