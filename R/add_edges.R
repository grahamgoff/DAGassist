#R/add_edges.R -Edge-addition exclusion-branch robustness.
#a missing arrow is a pathway the root DAG assumes is absent
#add_edges lets the user add specific hypothesized edges (directed (Z -> Y,
#a missing causal path) or bidirected (X <-> Y, an assumed latent common
#cause)) and reports if it changes the adjustment set, or breaks identification. 
#under the hood, each edge is its own branch DAG (i.e., root + specified edge).

#param parsing for add edges
.dagassist_parse_add_edges <- function(add_edges) {
  empty <- data.frame(from = character(0), to = character(0),
                      type = character(0), label = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(add_edges) || !length(add_edges)) return(empty)
  rows <- lapply(add_edges, function(s) {
    raw <- trimws(s)
    if (grepl("<->", raw, fixed = TRUE)) {            # must test <-> before -> / <-
      p <- trimws(strsplit(raw, "<->", fixed = TRUE)[[1]]); type <- "<->"; from <- p[1]; to <- p[2]
    } else if (grepl("->", raw, fixed = TRUE)) {
      p <- trimws(strsplit(raw, "->", fixed = TRUE)[[1]]); type <- "->";  from <- p[1]; to <- p[2]
    } else if (grepl("<-", raw, fixed = TRUE)) {
      p <- trimws(strsplit(raw, "<-", fixed = TRUE)[[1]]); type <- "->";  from <- p[2]; to <- p[1]  # canonicalize
    } else {
      stop(sprintf("Could not parse add_edges entry '%s'. Use 'A -> B', 'A <- B', or 'A <-> B'.", s), call. = FALSE)
    }
    if (length(p) != 2L || !nzchar(from) || !nzchar(to))
      stop(sprintf("Could not parse add_edges entry '%s'.", s), call. = FALSE)
    lab <- if (type == "<->") sprintf("%s <-> %s", from, to) else sprintf("%s -> %s", from, to)
    data.frame(from = from, to = to, type = type, label = lab, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#extract formula from formula param, engine call or fitted model w/o fitting.
.dagassist_formula_from_arg <- function(spec_expr, formula_val, eval_env) {
  if (is.null(spec_expr)) return(NULL)
  if (is.call(spec_expr) && !identical(spec_expr[[1L]], as.name("~"))) {
    fml <- tryCatch(.extract_from_engine_call(spec_expr, eval_env = eval_env)$formula,
                    error = function(e) NULL)
    if (!is.null(fml)) return(fml)
  }
  if (is.null(formula_val)) return(NULL)
  if (inherits(formula_val, "formula")) return(formula_val)
  tryCatch(stats::formula(formula_val), error = function(e) NULL)
}

#graph ops 
.dagassist_augment_dag <- function(dag, from, to, type) {
  struct <- .dagassist_structural_edges(dag)
  es <- if (nrow(struct)) sprintf("%s %s %s", struct$v, struct$e, struct$w) else character(0)
  es <- c(es, sprintf("%s %s %s", from, type, to))
  dagitty::dagitty(paste0("dag { ", paste(c(names(dag), es), collapse = "; "), " }"))
}

.dagassist_is_identifiable <- function(dag, exposure, outcome) {
  sets <- tryCatch(dagitty::adjustmentSets(dag, exposure = exposure, outcome = outcome, type = "minimal"),
                   error = function(e) NULL)
  !is.null(sets) && length(sets) > 0
}

#make core 
.dagassist_add_edges_robustness <- function(dag, exposure, outcome, edges_df, formula = NULL) {
  root_roles <- classify_nodes(dag, exposure, outcome)
  root_min   <- .minimal_sets_all(dag, exposure, outcome)
  root_canon <- .pick_canonical_controls(dag, exposure, outcome)
  root_ident <- length(root_min) > 0
  root_min_key <- .dagassist_family_key(root_min)
  root_canon_key <- .dagassist_family_key(list(root_canon))
  formula_vars <- if (!is.null(formula)) .rhs_terms_safe(formula) else character(0)
  struct <- .dagassist_structural_edges(dag)
  role_of <- function(rdf, v) { r <- rdf$role[rdf$variable == v]; if (length(r)) r[1] else NA_character_ }
  
  rows <- list()
  for (i in seq_len(nrow(edges_df))) {
    from <- edges_df$from[i]; to <- edges_df$to[i]; type <- edges_df$type[i]; lab <- edges_df$label[i]
    miss <- setdiff(c(from, to), names(dag))
    if (length(miss)) stop("add_edges references nodes not in the DAG: ", paste(miss, collapse = ", "),
                           ". Add them to the DAG first.", call. = FALSE)
    
    already <- if (!nrow(struct)) FALSE else if (type == "->")
      any(struct$e == "->" & struct$v == from & struct$w == to)
    else
      any(struct$e == "<->" & ((struct$v == from & struct$w == to) | (struct$v == to & struct$w == from)))
    
    aug <- .dagassist_augment_dag(dag, from, to, type)
    dagitty::exposures(aug) <- exposure; dagitty::outcomes(aug) <- outcome
    acyclic <- isTRUE(dagitty::isAcyclic(aug))
    
    row <- data.frame(edge = lab, identifiable = NA, minimal_changed = NA,
                      canonical_changed = NA, new_minimal = NA_character_,
                      role_changes = "", reestimate = NA, note = "",
                      stringsAsFactors = FALSE)
    if (already)  { row$note <- "edge already in DAG (no change)"; rows[[length(rows)+1L]] <- row; next }
    if (!acyclic) { row$note <- "adding this directed edge creates a cycle (invalid DAG)"; rows[[length(rows)+1L]] <- row; next }
    
    aug_min <- .minimal_sets_all(aug, exposure, outcome)
    aug_canon <- .pick_canonical_controls(aug, exposure, outcome)
    aug_ident <- length(aug_min) > 0
    aug_roles <- classify_nodes(aug, exposure, outcome)
    
    row$identifiable <- aug_ident
    row$minimal_changed <- !identical(.dagassist_family_key(aug_min), root_min_key)
    row$canonical_changed <- !identical(.dagassist_family_key(list(aug_canon)), root_canon_key)
    if (aug_ident && isTRUE(row$minimal_changed))
      row$new_minimal <- paste(vapply(aug_min, .format_set, character(1)), collapse = " | ")
    
    changed <- character(0); critical <- FALSE
    for (v in root_roles$variable) {
      r0 <- role_of(root_roles, v); r1 <- role_of(aug_roles, v)
      if (!is.na(r0) && !is.na(r1) && r0 != r1) {
        changed <- c(changed, sprintf("%s: %s->%s", v, r0, r1))
        if (xor(r0 %in% .DAGASSIST_BAD_ROLES, r1 %in% .DAGASSIST_BAD_ROLES)) critical <- TRUE
      }
    }
    row$role_changes <- paste(changed, collapse = "; ")
    ident_lost <- root_ident && !aug_ident
    crit_formula <- critical && any(vapply(changed, function(s) sub(":.*", "", s) %in% formula_vars, logical(1)))
    row$reestimate <- ident_lost || isTRUE(row$minimal_changed) || isTRUE(row$canonical_changed) || crit_formula
    rows[[length(rows)+1L]] <- row
  }
  
  per <- do.call(rbind, rows)
  structure(list(
    n_edges = nrow(edges_df),
    root = list(identifiable = root_ident, min = root_min, canon = root_canon),
    per_edge = per,
    reestimate = any(per$reestimate %in% TRUE) || any(per$identifiable %in% FALSE)
  ), class = "DAGassist_addedge_summary")
}

#printer 
#' Print an edge-addition (exclusion) robustness summary
#' @param x A `DAGassist_addedge_summary` from [add_edges_robustness()].
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
print.DAGassist_addedge_summary <- function(x, ...) {
  yn <- function(b) if (isTRUE(b)) "yes" else if (isFALSE(b)) "no" else "-"
  cat(clr_bold("\nEdge-addition (exclusion) robustness:"), "\n", sep = "")
  cat("- edges tested: ", x$n_edges, "\n", sep = "")
  for (i in seq_len(nrow(x$per_edge))) {
    e <- x$per_edge[i, ]
    if (nzchar(e$note)) { cat("  - ", e$edge, ": ", e$note, "\n", sep = ""); next }
    if (isFALSE(e$identifiable)) {
      cat("  - ", clr_red(e$edge), ": effect NOT identifiable if this pathway exists ",
          "(no adjustment set blocks it)\n", sep = ""); next
    }
    cat("  - ", e$edge, ": minimal changed: ", yn(e$minimal_changed),
        "; canonical changed: ", yn(e$canonical_changed), "\n", sep = "")
    if (isTRUE(e$minimal_changed) && !is.na(e$new_minimal))
      cat("        new minimal set(s): ", e$new_minimal, "\n", sep = "")
    if (nzchar(e$role_changes))
      cat("        role changes: ", e$role_changes, "\n", sep = "")
  }
  cat("- re-estimation recommended: ",
      if (isTRUE(x$reestimate)) clr_yellow("yes") else clr_green("no"), "\n", sep = "")
  invisible(x)
}

# ---- exported entry point ---------------------------------------------------
#' Diagnose robustness to added ("missing") edges
#'
#' Tests how the adjustment set and identification respond to edges the root DAG
#' assumes are absent (DAGWOOD exclusion branches). Each added edge is evaluated
#' as its own branch DAG (root + that edge).
#'
#' @param dag A `dagitty` DAG.
#' @param exposure,outcome Optional; inferred from the DAG when omitted.
#' @param add_edges Character vector like `c("Z -> Y", "X <-> Y")`. Directed
#'   (`->`, `<-`) and bidirected (`<->`, latent common cause) edges are supported.
#' @param formula Optional model formula or engine call; used only to decide
#'   whether a role-flipping covariate is in your specification.
#' @return A `DAGassist_addedge_summary` object.
#' @examplesIf requireNamespace("dagitty", quietly = TRUE)
#' g <- dagitty::dagitty("dag { Z->X; X->Y }")
#' dagitty::exposures(g) <- "X"; dagitty::outcomes(g) <- "Y"
#' add_edges_robustness(g, add_edges = c("Z -> Y", "X <-> Y"))
#' @export
add_edges_robustness <- function(dag, exposure, outcome, add_edges, formula = NULL) {
  spec_expr <- substitute(formula)
  fml <- .dagassist_formula_from_arg(spec_expr, formula, parent.frame())
  xy  <- .infer_xy(dag, if (missing(exposure)) NULL else exposure,
                   if (missing(outcome))  NULL else outcome)
  edges_df <- .dagassist_parse_add_edges(add_edges)
  if (!nrow(edges_df)) stop("Supply `add_edges`, e.g. c('Z -> Y', 'X <-> Y').", call. = FALSE)
  .dagassist_add_edges_robustness(dag, xy$exposure, xy$outcome, edges_df, formula = fml)
}