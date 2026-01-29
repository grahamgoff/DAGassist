#' Build a DirectEffects::sequential_g() formula from a DAG (and optionally a base model formula)
#'
#' @description
#' Returns a formula of the form `Y ~ A + X | Z | M` required by
#' [DirectEffects::sequential_g()], where:
#' - `A` is the treatment (exposure),
#' - `X` are baseline covariates,
#' - `Z` are intermediate (post-treatment) covariates affected by A that affect both M and Y,
#' - `M` are mediators (blip-down model terms).
#'
#' If `formula` is provided, `X` is (by default) constrained to the RHS terms that
#' appear in that base formula (mirroring DAGassist's current ACDE construction).
#'
#' @param dag A `dagitty` object (or a character DAGitty string).
#' @param formula Optional base formula (or an engine call like `fixest::feols(...)`).
#'   If supplied, the function uses its RHS to define candidate covariates and to
#'   pick up fixed-effects/grouping variables (which can be added to X).
#' @param data Optional data.frame. Used to (i) drop terms that do not exist in the data,
#'   and (ii) optionally drop covariates collinear with fixed effects (important for seqg stability).
#' @param exposure,outcome Optional. If missing, inferred from `dagitty::exposures()` / `dagitty::outcomes()`.
#' @param m,x,z Optional character vectors overriding inferred mediator (M), baseline (X),
#'   and intermediate (Z) terms.
#' @param include_descendants Logical; if TRUE, allow descendants of mediators to be included in M
#'   (conservatively restricted to descendants that remain ancestors of Y and descendants of A).
#' @param fe Optional character vector of FE/grouping variables to include in X (merged with any extracted from `formula`).
#' @param fe_as_factor Logical; if TRUE, wraps bare FE names as `factor(FE)` (only when FE exists in `data`).
#' @param drop_fe_collinear Logical; if TRUE and `data` is provided, drop any single-variable term
#'   that is constant within an FE group (DirectEffects is less forgiving than fixest here).
#' @param strict_rhs Logical; if TRUE and `formula` is provided, X is drawn only from RHS terms in `formula`.
#'   If FALSE, X is drawn from the DAG node set (pre-treatment ancestors) even if not in `formula`.
#' @param return_parts Logical; if TRUE, returns a list with components and metadata; otherwise returns just the formula.
#'
#' @return A formula (or a list with `$formula`, `$x`, `$z`, `$m`, `$fe`, `$dropped_fe`).
#' @export
seqg_formula <- function(
    dag,
    formula = NULL,
    data = NULL,
    exposure = NULL,
    outcome = NULL,
    m = NULL,
    x = NULL,
    z = NULL,
    include_descendants = FALSE,
    fe = NULL,
    fe_as_factor = TRUE,
    drop_fe_collinear = TRUE,
    strict_rhs = TRUE,
    return_parts = TRUE
) {
  # ---- DAG input ----
  if (is.character(dag)) {
    dag <- dagitty::dagitty(dag)
  }
  if (!inherits(dag, "dagitty")) {
    stop("`dag` must be a dagitty object (or a DAGitty string).", call. = FALSE)
  }
  
  # ---- Parse engine call (optional) ----
  base_fml <- NULL
  if (!is.null(formula)) {
    # Allow engine calls like fixest::feols(...)
    if (is.call(formula) && !.is_formula_expr(formula)) {
      parsed <- .extract_from_engine_call(formula, eval_env = parent.frame())
      base_fml <- parsed$formula
      if (is.null(data) && !is.null(parsed$data)) data <- parsed$data
    } else if (inherits(formula, "formula")) {
      base_fml <- formula
    } else {
      stop("`formula` must be a formula or an unevaluated engine call (e.g., fixest::feols(...)).", call. = FALSE)
    }
  }
  
  # ---- Infer exposure/outcome if needed ----
  xy <- .infer_xy(dag, exposure, outcome)
  exposure <- xy$exposure
  outcome  <- xy$outcome
  
  # sequential_g is written for a single treatment variable
  if (length(exposure) != 1L) {
    stop("`exposure` must be length 1 for sequential_g(). Provide a single treatment.", call. = FALSE)
  }
  A <- as.character(exposure)
  Y <- as.character(outcome)
  
  safe_desc <- function(node) tryCatch(dagitty::descendants(dag, node), error = function(e) character(0))
  safe_anc  <- function(node) tryCatch(dagitty::ancestors(dag, node),   error = function(e) character(0))
  
  # ---- Infer mediators M ----
  nodes <- names(dag)
  
  if (!is.null(m) && length(m)) {
    m_terms <- unique(as.character(m))
  } else {
    descA <- setdiff(safe_desc(A), A)
    ancY  <- safe_anc(Y)
    m_auto <- setdiff(intersect(descA, ancY), c(A, Y))
    
    if (isTRUE(include_descendants) && length(m_auto)) {
      # conservative: descendants of mediators that remain on the A->Y causal region
      descM <- unique(unlist(lapply(m_auto, function(mm) setdiff(safe_desc(mm), mm))))
      m_auto <- unique(c(m_auto, intersect(descM, intersect(descA, ancY))))
    }
    
    m_terms <- unique(m_auto)
  }
  
  if (!length(m_terms)) {
    stop(
      "No mediators inferred. Provide `m = c('M1','M2', ...)` or ensure mediators exist on the DAG path intersect(Desc(A), Anc(Y)).",
      call. = FALSE
    )
  }
  
  # ---- Infer intermediate covariates Z ----
  # Z in Desc(A) intersect Anc(M) intersect Anc(Y), excluding A/Y/M and post-mediator nodes
  ancY  <- safe_anc(Y)
  ancM  <- unique(unlist(lapply(m_terms, safe_anc)))
  descA <- setdiff(safe_desc(A), A)
  
  cand <- setdiff(nodes, c(A, Y, m_terms))
  z_auto <- intersect(cand, descA)
  z_auto <- intersect(z_auto, ancY)
  z_auto <- intersect(z_auto, ancM)
  
  # drop post-mediator nodes
  descM <- unique(unlist(lapply(m_terms, function(mm) setdiff(safe_desc(mm), mm))))
  z_auto <- setdiff(z_auto, descM)
  z_auto <- unique(z_auto)
  
  z_terms <- if (!is.null(z) && length(z)) unique(as.character(z)) else z_auto
  z_terms <- setdiff(z_terms, A)
  
  # ---- Infer baseline covariates X ----
  if (!is.null(x) && length(x)) {
    x_terms <- unique(as.character(x))
  } else {
    if (!is.null(base_fml) && isTRUE(strict_rhs)) {
      sp <- .strip_fixest_parts(base_fml)
      rhs_terms <- unique(.rhs_terms_safe(sp$base))
      x_terms <- setdiff(rhs_terms, c(A, m_terms, z_terms))
    } else {
      # DAG-based baseline set: pre-treatment ancestors of A/M/Y
      ancA <- safe_anc(A)
      ancY2 <- safe_anc(Y)
      ancM2 <- unique(unlist(lapply(m_terms, safe_anc)))
      pre <- setdiff(unique(c(ancA, ancY2, ancM2)), unique(c(descA, A, Y)))
      x_terms <- setdiff(pre, c(m_terms, z_terms))
    }
  }
  
  # ---- FE / grouping vars ----
  fe_vars <- character(0)
  if (!is.null(base_fml)) {
    fe_vars <- unique(c(fe_vars, .dagassist_extract_fe_vars(base_fml)))
  }
  if (!is.null(fe) && length(fe)) {
    fe_vars <- unique(c(fe_vars, as.character(fe)))
  }
  fe_vars <- setdiff(fe_vars, c("TRUE","FALSE","T","F","1","0"))
  
  # Only wrap factor() for bare symbols that exist as columns in data (if provided)
  factorize_plain_terms <- function(terms, data_names = NULL) {
    if (!length(terms)) return(character(0))
    is_bare <- grepl("^[.A-Za-z][.A-Za-z0-9._]*$", terms)
    if (!is.null(data_names)) {
      to_wrap <- is_bare & (terms %in% data_names)
    } else {
      to_wrap <- is_bare
    }
    terms[to_wrap] <- paste0("factor(", terms[to_wrap], ")")
    terms
  }
  
  dropped_fe <- character(0)
  if (length(fe_vars)) {
    data_names <- if (is.data.frame(data)) names(data) else NULL
    
    fe_terms <- if (isTRUE(fe_as_factor)) factorize_plain_terms(fe_vars, data_names) else fe_vars
    x_terms <- unique(c(x_terms, fe_terms))
    
    if (isTRUE(drop_fe_collinear) && is.data.frame(data)) {
      dx <- .dagassist_drop_terms_collinear_with_fe(x_terms, data, fe_vars)
      x_terms <- dx$keep
      
      dz <- .dagassist_drop_terms_collinear_with_fe(z_terms, data, fe_vars)
      z_terms <- dz$keep
      
      dropped_fe <- unique(c(dx$dropped, dz$dropped))
    }
  }
  
  # ---- If data is supplied, drop terms that don't resolve to data columns ----
  if (is.data.frame(data)) {
    data_names <- names(data)
    x_terms <- .dagassist_terms_must_use_data(x_terms, data_names)
    z_terms <- .dagassist_terms_must_use_data(z_terms, data_names)
    m_terms <- .dagassist_terms_must_use_data(m_terms, data_names)
  }
  
  if (!length(m_terms)) {
    stop("After filtering, mediator terms are empty. Provide `m=` explicitly and/or check `data` column names.", call. = FALSE)
  }
  
  # ---- Build final sequential_g formula ----
  block1 <- paste(c(A, x_terms), collapse = " + ")
  if (!nzchar(block1)) block1 <- A
  
  # IMPORTANT: when Z is empty, use "0" (not "1") to avoid degenerate first-stage structure
  block2 <- if (length(z_terms)) paste(z_terms, collapse = " + ") else "0"
  block3 <- paste(m_terms, collapse = " + ")
  
  f_str <- paste0(Y, " ~ ", block1, " | ", block2, " | ", block3)
  f_out <- stats::as.formula(f_str, env = if (!is.null(base_fml)) environment(base_fml) else parent.frame())
  
  if (!isTRUE(return_parts)) return(f_out)
  
  list(
    formula = f_out,
    x = x_terms,
    z = z_terms,
    m = m_terms,
    fe = fe_vars,
    dropped_fe = dropped_fe
  )
}