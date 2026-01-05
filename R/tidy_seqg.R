# tidy_seqg.R
# broom tidier + vcov for DirectEffects::sequential_g output ("seqg")

#' @method vcov seqg
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
  
  ##safe catch if directeffects not loaded
  seq_g_vcov <- tryCatch(
    get("seq_g_vcov", envir = asNamespace("DirectEffects")),
    error = function(e) NULL
  )
  
  if (is.null(seq_g_vcov) || !is.function(seq_g_vcov)) {
    stop(
      "Could not locate DirectEffects' internal function seq_g_vcov(). Please update DirectEffects or use summary(x, ...) for inference.",
      call. = FALSE
    )
  }
  
  seq_g_vcov(
    first_mod = object$first_mod,
    direct_mod = object,
    X1 = X1,
    X2 = X2,
    med.vars = med.vars
  )
}

#' @method tidy seqg
#' @importFrom broom tidy
#' @export
tidy.seqg <- function(x,
                      conf.int = FALSE,
                      conf.level = 0.95,
                      ...) {
  
  # Prefer DirectEffects' own inference (same machinery used by summary.seqg())
  sm <- tryCatch(suppressWarnings(summary(x, ...)), error = function(e) NULL)
  
  mat <- NULL
  if (inherits(sm, "coeftest")) {
    mat <- as.matrix(sm)
  } else if (is.matrix(sm)) {
    mat <- sm
  } else if (is.list(sm) && !is.null(sm$coefficients)) {
    mat <- as.matrix(sm$coefficients)
  }
  
  if (!is.null(mat) && nrow(mat) > 0) {
    cn <- colnames(mat)
    
    pick1 <- function(cands) {
      hit <- which(cn %in% cands)
      if (length(hit)) hit[1] else NA_integer_
    }
    
    i_est  <- pick1(c("Estimate", "estimate"))
    i_se   <- pick1(c("Std. Error", "Std. Err.", "Std.Err.", "std.error", "SE"))
    i_stat <- pick1(c("t value", "z value", "statistic"))
    i_p    <- pick1(c("Pr(>|t|)", "Pr(>|z|)", "p.value"))
    
    # Fallback: assume first four columns are (est, se, stat, p) like coeftest output
    if (is.na(i_est))  i_est  <- 1
    if (is.na(i_se))   i_se   <- if (ncol(mat) >= 2) 2 else NA_integer_
    if (is.na(i_stat)) i_stat <- if (ncol(mat) >= 3) 3 else NA_integer_
    if (is.na(i_p))    i_p    <- if (ncol(mat) >= 4) 4 else NA_integer_
    
    out <- data.frame(
      term      = rownames(mat),
      estimate  = as.numeric(mat[, i_est]),
      std.error = if (!is.na(i_se))   as.numeric(mat[, i_se])   else NA_real_,
      statistic = if (!is.na(i_stat)) as.numeric(mat[, i_stat]) else NA_real_,
      p.value   = if (!is.na(i_p))    as.numeric(mat[, i_p])    else NA_real_,
      stringsAsFactors = FALSE
    )
    
  } else {
    # Last-resort fallback (should almost never trigger now)
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
    V <- tryCatch(stats::vcov(x), error = function(e) NULL)
    se <- rep(NA_real_, length(co))
    names(se) <- names(co)
    if (!is.null(V) && is.matrix(V)) {
      d <- sqrt(diag(V))
      if (!is.null(names(d))) se[names(co)] <- d[names(co)]
    }
    out <- data.frame(
      term      = names(co),
      estimate  = unname(co),
      std.error = unname(se),
      statistic = NA_real_,
      p.value   = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  
  # Optional conf int (Normal approx; modelsummary doesn't require this)
  if (isTRUE(conf.int)) {
    alpha <- 1 - conf.level
    crit  <- stats::qnorm(1 - alpha / 2)
    ok <- is.finite(out$std.error) & out$std.error > 0
    out$conf.low  <- NA_real_
    out$conf.high <- NA_real_
    out$conf.low[ok]  <- out$estimate[ok] - crit * out$std.error[ok]
    out$conf.high[ok] <- out$estimate[ok] + crit * out$std.error[ok]
  }
  
  # Respect DAGassist meta filter if you ever attach it
  meta <- attr(x, "dagassist_meta", exact = TRUE)
  if (is.list(meta) && isTRUE(meta$tidy_only_exposure) && !is.null(meta$exposure)) {
    out <- out[out$term %in% meta$exposure, , drop = FALSE]
  }
  
  rownames(out) <- NULL
  out
}