###############################################################################
# GET THE CLUSTERED STANDARD ERRORS
###############################################################################
#so it works with modelsummary
#custon wrap for seqg to prevent s3 issues
.dagassist_wrap_seqg <- function(coef, vcov, nobs, df_resid = NA_integer_) {
  structure(
    list(
      coefficients = coef,
      vcov = vcov,
      nobs = as.integer(nobs),
      df.residual = as.integer(df_resid)
    ),
    class = "dagassist_seqg"
  )
}
##handle weird ci break
#' @importFrom stats as.formula coef lm model.matrix pt qt residuals vcov
NULL

#' @exportS3Method stats::coef
coef.dagassist_seqg <- function(object, ...) object$coefficients

#' @exportS3Method stats::vcov
vcov.dagassist_seqg <- function(object, ...) object$vcov

#' @exportS3Method stats::nobs
nobs.dagassist_seqg <- function(object, ...) object$nobs

#' @exportS3Method broom::tidy
tidy.dagassist_seqg <- function(x, ...) {
  b <- x$coefficients
  V <- x$vcov
  se <- sqrt(diag(V))
  tval <- b / se
  
  # If you pass CR2, you’ll typically want df = G - 1.
  # Store it as attr(vcov, "df") if you want; otherwise fall back to Normal.
  df <- attr(V, "df", exact = TRUE)
  if (is.null(df) || !is.finite(df)) {
    p <- 2 * stats::pnorm(abs(tval), lower.tail = FALSE)
  } else {
    p <- 2 * stats::pt(abs(tval), df = df, lower.tail = FALSE)
  }
  
  data.frame(
    term = names(b),
    estimate = unname(b),
    std.error = unname(se),
    statistic = unname(tval),
    p.value = unname(p),
    stringsAsFactors = FALSE
  )
}

#' @exportS3Method broom::glance
glance.dagassist_seqg <- function(x, ...) {
  data.frame(
    nobs = x$nobs,
    df.residual = x$df.residual,
    r.squared = NA_real_,
    adj.r.squared = NA_real_,
    stringsAsFactors = FALSE
  )
}

# ============================================================
# HELPERS
# ============================================================

.get_mediator_cols_X1 <- function(object, X1) {
  dat <- object$model
  
  tl <- attr(object$terms$M, "term.labels")
  if (length(tl) == 0) stop("No mediator terms found in object$terms$M")
  
  f_med <- stats::as.formula(paste("~", paste(tl, collapse = " + ")))
  MM <- stats::model.matrix(f_med, data = dat)
  
  if ("(Intercept)" %in% colnames(MM)) {
    MM <- MM[, colnames(MM) != "(Intercept)", drop = FALSE]
  }
  
  # Prefer exact column matches (works for interactions, transforms, factors expanded by model.matrix)
  idx <- which(colnames(X1) %in% colnames(MM))
  
  # Fallback: fuzzy match on underlying variable names
  if (length(idx) == 0) {
    med_vars <- unique(all.vars(f_med))
    pat <- paste(med_vars, collapse = "|")
    idx <- which(grepl(pat, colnames(X1)))
  }
  
  if (length(idx) == 0) stop("Could not identify mediator columns in X1.")
  idx
}

# --- stable symmetric inverse square-root ---
.mat_sqrt_inv <- function(A, tol = 1e-10) {
  eig <- eigen(A, symmetric = TRUE)
  vals <- pmax(eig$values, tol)
  eig$vectors %*% diag(1 / sqrt(vals), length(vals)) %*% t(eig$vectors)
}

# --- stable symmetric inverse square-root ---
.safe_sqrt_inv <- function(IminusH, tol = 1e-10, max_tries = 6, max_ridge = 1e6) {
  ng <- nrow(IminusH)
  ridge <- 0
  
  for (t in seq_len(max_tries)) {
    A <- IminusH
    if (ridge > 0) diag(A) <- diag(A) + ridge
    
    eig <- tryCatch(eigen(A, symmetric = TRUE), error = function(e) NULL)
    if (is.null(eig) || any(!is.finite(eig$values))) {
      ridge <- if (ridge == 0) tol else ridge * 10
      if (ridge > max_ridge) break
      next
    }
    
    min_e <- min(eig$values)
    if (min_e < tol) {
      # add just enough ridge to push the minimum eigenvalue above tol
      ridge <- ridge + (tol - min_e) + tol
      if (ridge > max_ridge) break
      next
    }
    
    invsqrt <- eig$vectors %*% diag(1 / sqrt(eig$values), ng) %*% t(eig$vectors)
    return(list(A = invsqrt, ridge = ridge, ok = TRUE))
  }
  
  # hard fallback: no adjustment
  list(A = diag(ng), ridge = ridge, ok = FALSE)
}

# ============================================================
# ACTUAL ESTIMATION of CLUSTERED ROBUST SEs (DEFAULT: CR2)
# ============================================================

.dagassist_vcov_seqg_clustered <- function(object, cluster,
                                type = c("CR2", "CR1S", "CR1", "CR0"),
                                hc   = c("HC1", "HC0"),
                                Fhat_mode = c("weighted", "package"),
                                tol = 1e-10,
                                warn_fallback = TRUE) {
  type <- match.arg(type)
  hc   <- match.arg(hc)
  Fhat_mode <- match.arg(Fhat_mode)
  stopifnot(inherits(object, "seqg"))
  
  # drop aliased columns
  X2 <- object$X[, !object$aliased, drop = FALSE]
  X1 <- object$first_mod$XZM[, !object$first_mod$aliased, drop = FALSE]
  
  n <- nrow(X2)
  p <- ncol(X2)
  
  if (length(cluster) != n) stop("cluster must have length nrow(X2)")
  cl <- as.factor(cluster)
  G  <- nlevels(cl)
  if (G < 2) stop("Need at least 2 clusters")
  
  # residuals
  e2 <- as.numeric(stats::residuals(object))
  e1 <- as.numeric(stats::residuals(object$first_mod))
  
  # stage weights (usually identical when passed via sequential_g(weights=...),
  # but we read them separately to be safe)
  w2 <- object$weights
  w1 <- object$first_mod$weights
  
  sw2 <- if (is.null(w2)) rep(1, n) else sqrt(as.numeric(w2))
  sw1 <- if (is.null(w1)) rep(1, n) else sqrt(as.numeric(w1))
  
  # transformed designs and residuals (WLS geometry)
  X2s <- X2 * sw2
  X1s <- X1 * sw1
  e2s <- e2 * sw2
  e1s <- e1 * sw1
  
  # mediator cols
  med_cols <- .get_mediator_cols_X1(object, X1)
  
  # Fhat (package-style vs weighted)
  if (Fhat_mode == "package" || is.null(w2)) {
    Fhat <- crossprod(X2, X1) / n
  } else {
    # weighted cross-moment using SECOND-STAGE weights
    X2F <- X2 * sw2
    X1F <- X1 * sw2
    Fhat <- crossprod(X2F, X1F) / n
  }
  Fhat[, -med_cols] <- 0
  
  # bread matrices in transformed spaces
  qr2 <- qr(X2s)
  if (qr2$rank != p) stop("Second-stage design not full rank after dropping aliased cols.")
  bread2 <- chol2inv(qr2$qr[1:p, 1:p, drop = FALSE])
  
  k1 <- ncol(X1s)
  qr1 <- qr(X1s)
  if (qr1$rank != k1) stop("First-stage design not full rank after dropping aliased cols.")
  bread1 <- chol2inv(qr1$qr[1:k1, 1:k1, drop = FALSE])
  
  # ghat block builder: u2/u1 are *transformed residual vectors* (e*s) possibly CR2-adjusted
  ghat_block <- function(idx, u2_adj, u1_adj) {
    efun2 <- sweep(X2s[idx, , drop = FALSE], 1, u2_adj, "*")
    efun1 <- sweep(X1s[idx, , drop = FALSE], 1, u1_adj, "*")
    t(efun2) + Fhat %*% bread1 %*% t(efun1)
  }
  
  # diagnostics for CR2 fallback
  diag_info <- list(
    n = n, p = p, G = G,
    Fhat_mode = Fhat_mode,
    ridge2 = 0, ridge1 = 0,
    ident2 = 0, ident1 = 0
  )
  
  if (type == "CR2") {
    meat <- matrix(0, p, p)
    levs <- levels(cl)
    
    for (g in seq_len(G)) {
      idx <- which(cl == levs[g])
      ng  <- length(idx)
      
      # stage-2 Hgg in transformed space
      H2 <- X2s[idx, , drop = FALSE] %*% bread2 %*% t(X2s[idx, , drop = FALSE])
      res2 <- .safe_sqrt_inv(diag(ng) - H2, tol = tol)
      A2 <- res2$A
      diag_info$ridge2 <- diag_info$ridge2 + res2$ridge
      if (!res2$ok) diag_info$ident2 <- diag_info$ident2 + 1L
      
      # stage-1 Hgg in transformed space
      H1 <- X1s[idx, , drop = FALSE] %*% bread1 %*% t(X1s[idx, , drop = FALSE])
      res1 <- .safe_sqrt_inv(diag(ng) - H1, tol = tol)
      A1 <- res1$A
      diag_info$ridge1 <- diag_info$ridge1 + res1$ridge
      if (!res1$ok) diag_info$ident1 <- diag_info$ident1 + 1L
      
      # adjusted transformed residual vectors
      u2_adj <- as.numeric(A2 %*% e2s[idx])
      u1_adj <- as.numeric(A1 %*% e1s[idx])
      
      ghat_g <- ghat_block(idx, u2_adj, u1_adj)   # p x ng
      s_g    <- rowSums(ghat_g)                   # p-vector
      meat   <- meat + tcrossprod(s_g)
    }
    
    V <- bread2 %*% meat %*% bread2
    
  } else {
    # CR0/CR1/CR1S
    ghat <- ghat_block(seq_len(n), e2s, e1s)      # p x n
    U    <- t(ghat)                                # n x p
    gsum <- rowsum(U, cl, reorder = TRUE)          # G x p
    meat <- crossprod(gsum)
    V <- bread2 %*% meat %*% bread2
  }
  
  # HC scaling (two-stage-ish)
  if (hc == "HC1") {
    k_df <- max(object$rank, object$first_mod$rank)
    V <- (n / (n - k_df)) * V
  }
  
  # scalar cluster adjustment
  adj <- switch(type,
                "CR0"  = 1,
                "CR1"  = G / (G - 1),
                "CR1S" = (G / (G - 1)) * ((n - 1) / (n - p)),
                "CR2"  = 1)
  
  V <- adj * V
  dimnames(V) <- list(colnames(X2), colnames(X2))
  
  attr(V, "seqg_clustered_diagnostics") <- diag_info
  
  if (type == "CR2" && warn_fallback && (diag_info$ident2 > 0 || diag_info$ident1 > 0)) {
    warning(sprintf(
      "CR2 fallback used: identity A_g for %d clusters (stage2) and %d clusters (stage1). Consider CR1S or wild cluster bootstrap.",
      diag_info$ident2, diag_info$ident1
    ))
  }
  V
}

# ============================================================
# Validate that everything went through
# ============================================================

.validate_vcov <- function(object) { 
  
  cat("=== Validation: Checking two-stage variance formula ===\n\n")
  V_default <- stats::vcov(object)
  se_default <- sqrt(diag(V_default))
  n <- length(stats::residuals(object))
  cluster_individual <- 1:n
  V_ours <- .dagassist_vcov_seqg_clustered(object, cluster_individual, type = "CR0", hc = "HC1")
  se_ours <- sqrt(diag(V_ours))
  comparison <- data.frame(
    Coefficient = names(se_default),
    SE_DirectEffects = round(se_default, 4),
    SE_Ours = round(se_ours, 4),
    Ratio = round(se_ours / se_default, 4)
  )
  print(comparison)
  cat("\nNote: Ratios close to 1.0 indicate our formula matches DirectEffects.\n")
  return(invisible(comparison))
}

# ============================================================
# Make CR2 the default in summary wrapper
# ============================================================

# Make CR2 the default in your summary wrapper too:
.summary_seqg_clustered <- function(seq_g_obj, cluster,
                                   type = "CR2", hc = "HC1",
                                   conf_level = 0.95) {
  vcov_cl <- .dagassist_vcov_seqg_clustered(seq_g_obj, cluster, type = type, hc = hc)
  
  beta <- stats::coef(seq_g_obj)
  se_cl <- sqrt(diag(vcov_cl))
  se_orig <- sqrt(diag(stats::vcov(seq_g_obj)))
  
  G <- nlevels(as.factor(cluster))
  df_adj <- G - 1
  
  t_stat <- beta / se_cl
  p_val <- 2 * stats::pt(-abs(t_stat), df = df_adj)
  
  ci_mult <- stats::qt(1 - (1 - conf_level) / 2, df = df_adj)
  ci_lo <- beta - ci_mult * se_cl
  ci_hi <- beta + ci_mult * se_cl
  
  structure(
    list(
      coefficients = beta,
      se_clustered = se_cl,
      se_original = se_orig,
      vcov_clustered = vcov_cl,
      t_stat = t_stat,
      p_value = p_val,
      ci_lower = ci_lo,
      ci_upper = ci_hi,
      n_obs = length(stats::residuals(seq_g_obj)),
      n_clusters = G,
      df = df_adj,
      conf_level = conf_level,
      type = type,
      hc = hc
    ),
    class = "summary_seqg_clustered"
  )
}

# ============================================================
# Print method
# ============================================================

.print.summary_seqg_clustered <- function(x, digits = 4, ...) {
  cat("\n")
  cat("Sequential G-Estimation: Controlled Direct Effect\n")
  cat("With Clustered Robust Standard Errors\n")
  cat("=================================================\n\n")
  
  cat(sprintf("Observations: %d\n", x$n_obs))
  cat(sprintf("Clusters: %d\n", x$n_clusters))
  cat(sprintf("Cluster adjustment: %s\n", x$type))
  cat(sprintf("Degrees of freedom: %d (G - 1)\n\n", x$df))
  
  stars <- ifelse(x$p_value < 0.001, "***",
                  ifelse(x$p_value < 0.01, "**",
                         ifelse(x$p_value < 0.05, "*",
                                ifelse(x$p_value < 0.1, ".", ""))))
  
  results_df <- data.frame(
    Estimate = round(x$coefficients, digits),
    SE_Orig = round(x$se_original, digits),
    SE_Clust = round(x$se_clustered, digits),
    t_value = round(x$t_stat, digits),
    p_value = formatC(x$p_value, digits = digits, format = "g"),
    CI_Lo = round(x$ci_lower, digits),
    CI_Hi = round(x$ci_upper, digits),
    Sig = stars,
    stringsAsFactors = FALSE
  )
  rownames(results_df) <- names(x$coefficients)
  
  print(results_df)
  
  cat("\n---\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  cat(sprintf("\n%d%% CIs based on t(%d) distribution\n",
              round(x$conf_level * 100), x$df))
  
  cat("\nSE Inflation Ratio (Clustered / Original):\n")
  ratio <- x$se_clustered / x$se_original
  names(ratio) <- x$coef_names
  print(round(ratio, 3))
  
  invisible(x)
}

# ============================================================
# Convert seqg class to lm (supported by marginaleffects)
# ============================================================

.seqg_stage2_lm <- function(seqg_obj) {
  stopifnot(inherits(seqg_obj, "seqg"))
  
  dat <- seqg_obj$model
  if (is.null(dat)) stop("seqg_obj$model is missing; need model frame stored in the seqg object.")
  
  # The de-mediated outcome used for the stage-2 regression:
  if (is.null(seqg_obj$Ytilde)) stop("seqg_obj$Ytilde not found in the seqg object.")
  dat[[".ytilde"]] <- as.numeric(seqg_obj$Ytilde)
  
  # Build RHS for stage 2 from seqg terms
  tt  <- seqg_obj$terms$X
  rhs <- paste(attr(tt, "term.labels"), collapse = " + ")
  if (!nzchar(rhs)) rhs <- "1"
  if (attr(tt, "intercept") == 0) rhs <- paste("0 +", rhs)
  
  f2 <- as.formula(paste(".ytilde ~", rhs))
  
  # Fit an lm that replicates stage 2 (same design/weights/contrasts)
  lm(f2, data = dat, weights = seqg_obj$weights, contrasts = seqg_obj$contrasts)
}

# optional sanity check: coefficients should match
.check_stage2_match <- function(seqg_obj) {
  lm2 <- .seqg_stage2_lm(seqg_obj)
  ok <- isTRUE(all.equal(unname(stats::coef(lm2)), unname(stats::coef(seqg_obj)), tolerance = 1e-8))
  if (!ok) warning("Stage-2 lm coefficients do not match seqg coefficients (check contrasts / aliased columns).")
  invisible(ok)
}

####################### BIG SACDE WORKFLOW #####################################
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


.dagassist_add_sacde_models <- function(x, mods) {
  ##functions for helpful fail if error gets passed to vcov helper
  .is_fit_error <- function(obj) inherits(obj, "DAGassist_fit_error")
  .fit_error_msg <- function(obj) {
    if (!is.null(obj$error)) return(obj$error)     # <-- THIS is what .safe_fit stores
    if (!is.null(obj$message)) return(obj$message)
    "Fit failed (no message captured)."
  }
  
  if (!requireNamespace("DirectEffects", quietly = TRUE)) {
    stop("SACDE requires the 'DirectEffects' package.", call. = FALSE)
  }
  if (!requireNamespace("WeightIt", quietly = TRUE)) {
    stop("SACDE requires the 'WeightIt' package.", call. = FALSE)
  }
  
  data0 <- x$.__data
  if (is.null(data0)) {
    stop("SACDE requires calling DAGassist(..., data = <data.frame>).", call. = FALSE)
  }
  
  exp_nm <- get_by_role(x$roles, "exposure")
  out_nm <- get_by_role(x$roles, "outcome")
  
  # ---- choose Canonical spec ONLY ----
  if (!("Canonical" %in% names(mods))) {
    stop(
      "SACDE requires a Canonical model column to exist.\n",
      "DAGassist could not find a 'Canonical' model among the fitted specs.",
      call. = FALSE
    )
  }
  base_fml <- .dagassist_formula_for_model_name(x, "Canonical")
  if (is.null(base_fml)) stop("Could not recover Canonical formula for SACDE.", call. = FALSE)
  
  # cluster vars from engine args (fixest-style)
  engine_args <- x$settings$engine_args
  cluster_vars <- character(0)
  if (is.list(engine_args)) {
    if ("cluster" %in% names(engine_args)) {
      cl <- engine_args$cluster
      if (inherits(cl, "formula")) cluster_vars <- c(cluster_vars, all.vars(cl))
      if (is.character(cl) && length(cl) == 1L) cluster_vars <- c(cluster_vars, cl)
    }
    if ("clusters" %in% names(engine_args)) {
      cl <- engine_args$clusters
      if (inherits(cl, "formula")) cluster_vars <- c(cluster_vars, all.vars(cl))
      if (is.character(cl) && length(cl) == 1L) cluster_vars <- c(cluster_vars, cl)
    }
  }
  cluster_vars <- unique(cluster_vars)
  
  # WeightIt args (reuse existing normalization)
  wargs <- .dagassist_normalize_weights_args(x$settings$weights_args)
  trim_at <- wargs[["trim_at"]]
  wargs[["trim_at"]] <- NULL
  fa <- .dagassist_filter_args(wargs, WeightIt::weightit)
  
  # sequential_g args (user-supplied)
  de_args <- x$settings$directeffects_args
  if (is.null(de_args)) de_args <- list()
  
  # Build base ACDE formula from Canonical spec
  acde <- .dagassist_normalize_acde_spec(x$settings$acde)
  f_seqg0 <- .dagassist_build_acde_formula(base_fml, x, acde)
  
  # Parse blocks
  blocks <- .dagassist_parse_seqg_formula(f_seqg0)
  
  # Mediator terms (original), then create centered + squared versions (like seqg_functions.R)
  m_terms <- attr(f_seqg0, "dagassist_acde_m_terms", exact = TRUE)
  if (is.null(m_terms) || !length(m_terms)) {
    stop("SACDE requested, but no mediators were inferred for the Canonical spec.", call. = FALSE)
  }
  
  # vars needed for complete-case sample: everything in f_seqg0 + cluster vars
  vars_need <- unique(c(all.vars(f_seqg0), cluster_vars))
  vars_need <- intersect(vars_need, names(data0))
  
  data_cc <- stats::na.omit(data0[, vars_need, drop = FALSE])
  if (!nrow(data_cc)) stop("SACDE complete-case sample is empty after NA dropping.", call. = FALSE)
  
  # Guard: mediators must be numeric for polynomial expansion
  bad_m <- m_terms[!vapply(m_terms, function(nm) is.numeric(data_cc[[nm]]), logical(1))]
  if (length(bad_m)) {
    stop(
      "SACDE requires numeric mediator(s) for polynomial expansion. Non-numeric mediator(s): ",
      paste(bad_m, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Create centered mediator columns; add squared term ONLY when mediator is non-binary
  m_poly_terms <- character(0)
  
  for (m in m_terms) {
    x <- data_cc[[m]]
    
    # unique non-missing support
    u <- unique(x[is.finite(x)])
    k <- length(u)
    
    # drop constant mediators entirely (they contribute nothing and can cause singularity noise)
    if (k <= 1L) next
    
    mc <- paste0(m, "_c")
    data_cc[[mc]] <- x - mean(x, na.rm = TRUE)
    m_poly_terms <- c(m_poly_terms, mc)
    
    # only include quadratic term for genuinely multi-valued mediators
    if (k > 2L) {
      mc2 <- paste0(m, "_c2")
      data_cc[[mc2]] <- data_cc[[mc]]^2
      m_poly_terms <- c(m_poly_terms, mc2)
    }
  }
  
  # Rebuild seqg formula: same blocks, but mediator block uses poly terms
  # Also: if no Z terms (blockZ == "0"), use "1" to mirror your professor’s script style.
  blockZ_use <- if (identical(trimws(blocks$blockZ), "0")) "1" else blocks$blockZ
  f_seqg <- stats::as.formula(
    paste0(blocks$y, " ~ ", blocks$block1, " | ", blockZ_use, " | ", paste(m_poly_terms, collapse = " + "))
  )
  
  # Baseline terms for weights come from block1 minus exposure
  f_block1 <- stats::as.formula(paste("~", blocks$block1))
  baseline_terms <- setdiff(all.vars(f_block1), exp_nm)
  
  f_treat <- if (length(baseline_terms)) {
    stats::as.formula(paste(exp_nm, "~", paste(baseline_terms, collapse = " + ")))
  } else {
    stats::as.formula(paste(exp_nm, "~ 1"))
  }
  
  # Fit raw seqg (Raw (SACDE))
  seqg_raw <- .safe_fit(DirectEffects::sequential_g, f_seqg, data_cc, de_args)
  
  if (.is_fit_error(seqg_raw)) {
    stop(
      "SACDE raw sequential_g estimation failed.\n\n",
      "Underlying error: ", .fit_error_msg(seqg_raw), "\n\n",
      "seqg formula:\n  ", paste(deparse(f_seqg), collapse = ""),
      call. = FALSE
    )
  }
  
  # WeightIt on same complete-case sample; do NOT weight on mediators (they are not in f_treat)
  wtobj <- suppressWarnings(
    do.call(
      WeightIt::weightit,
      c(
        list(formula = f_treat, data = data_cc, method = "glm", estimand = "ATE"),
        fa$keep
      )
    )
  )
  w <- as.numeric(wtobj$weights)
  
  if (!is.null(trim_at)) {
    cap <- as.numeric(stats::quantile(w, probs = trim_at, na.rm = TRUE, names = FALSE))
    w <- pmin(w, cap)
  }
  
  # Fit weighted seqg (Weighted (SACDE))
  seqg_w <- .safe_fit(
    DirectEffects::sequential_g,
    f_seqg,
    data_cc,
    c(de_args, list(weights = w))
  )
  
  if (.is_fit_error(seqg_w)) {
    stop(
      "SACDE weighted sequential_g estimation failed.\n\n",
      "Underlying error: ", .fit_error_msg(seqg_w), "\n\n",
      "seqg formula:\n  ", paste(deparse(f_seqg), collapse = ""),
      call. = FALSE
    )
  }
  
  #robust n for seqg objects. previously, would crash if there was not a cluster argument
  .seqg_n <- function(obj) {
    X <- obj$X
    if (!is.null(X) && !is.null(dim(X))) return(nrow(X))
    # fallback: residuals length is always n
    length(stats::residuals(obj))
  }
  
  if (length(cluster_vars)) {
    cl0 <- cluster_vars[1]
    
    # align cluster to the rows used by sequential_g
    idx <- match(rownames(seqg_w$X), rownames(data_cc))
    cl_vec <- data_cc[[cl0]][idx]
    
    V_raw <- .dagassist_vcov_seqg_clustered(seqg_raw, cluster = cl_vec, type = "CR2")
    V_w   <- .dagassist_vcov_seqg_clustered(seqg_w,   cluster = cl_vec, type = "CR2")
    
    df_cl <- nlevels(as.factor(cl_vec)) - 1
    attr(V_raw, "df") <- df_cl
    attr(V_w,   "df") <- df_cl
    
  } else {
    # No cluster requested:
    # Don't rely on vcov.seqg being registered; use our two-stage sandwich via
    # "individual clusters" (matches DirectEffects default in .validate_vcov()).
    n_raw <- .seqg_n(seqg_raw)
    n_w   <- .seqg_n(seqg_w)
    
    V_raw <- .dagassist_vcov_seqg_clustered(seqg_raw, cluster = seq_len(n_raw),
                                            type = "CR0", hc = "HC1")
    V_w   <- .dagassist_vcov_seqg_clustered(seqg_w,   cluster = seq_len(n_w),
                                            type = "CR0", hc = "HC1")
  }
  
  # Wrap models (use robust n)
  m_raw <- .dagassist_wrap_seqg(stats::coef(seqg_raw), V_raw, nobs = .seqg_n(seqg_raw))
  m_w   <- .dagassist_wrap_seqg(stats::coef(seqg_w),   V_w,   nobs = .seqg_n(seqg_w))
  
  # Insert exactly two columns after Canonical (SATE) if present, else after Canonical
  insert_after <- if ("Canonical (SATE)" %in% names(mods)) "Canonical (SATE)" else "Canonical"
  
  out <- list()
  for (nm in names(mods)) {
    out[[nm]] <- mods[[nm]]
    if (identical(nm, insert_after)) {
      out[["Raw (SACDE)"]]      <- m_raw
      out[["Weighted (SACDE)"]] <- m_w
    }
  }
  
  out
}