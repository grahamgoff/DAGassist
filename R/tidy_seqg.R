#' Tidy a DirectEffects sequential_g model (class 'seqg')
#'
#' @param x A seqg object from DirectEffects::sequential_g()
#' @param conf.int Logical; include confidence intervals
#' @param conf.level Confidence level
#' @param ... Unused
#' @export
tidy.seqg <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  s <- tryCatch(summary(x), error = function(e) NULL)
  
  # Try summary() first (most likely includes a coefficient table)
  co <- NULL
  if (!is.null(s) && !is.null(s$coefficients)) co <- s$coefficients
  
  if (!is.null(co) && is.matrix(co) && ncol(co) >= 2) {
    out <- data.frame(
      term      = rownames(co),
      estimate  = unname(co[, 1]),
      std.error = unname(co[, 2]),
      stringsAsFactors = FALSE
    )
    
    # Optional columns if present
    if (ncol(co) >= 3) out$statistic <- unname(co[, 3])
    if (ncol(co) >= 4) out$p.value   <- unname(co[, 4])
    
  } else {
    # Fallback: coefficients + (optional) vcov
    b <- tryCatch(stats::coef(x), error = function(e) x$coefficients)
    if (is.null(b)) stop("Could not extract coefficients from seqg object.", call. = FALSE)
    
    V <- tryCatch(stats::vcov(x), error = function(e) NULL)
    se <- if (!is.null(V)) sqrt(diag(V)) else rep(NA_real_, length(b))
    
    out <- data.frame(
      term      = names(b),
      estimate  = as.numeric(b),
      std.error = as.numeric(se),
      stringsAsFactors = FALSE
    )
  }
  
  if (isTRUE(conf.int)) {
    alpha <- 1 - conf.level
    crit <- stats::qnorm(1 - alpha/2)
    out$conf.low  <- out$estimate - crit * out$std.error
    out$conf.high <- out$estimate + crit * out$std.error
  }
  
  out
}