################################################################################
################ FEATURES FOR EDGE UNCERTAINTY USING PDAGS #####################
################################################################################

####normal DAGassist calls work off of a single dagitty object. this makes adding
####uncertainty via a string parameter tricky, because it would entail processing
####two different, potentially conflicting, graphs. the solution is to convert
####DAGs into PDAGs when there is an uncertainty parameter, accepting either a
####dagitty PDAG object or a DAG object + uncertainty string. 
####these first four functions normalize the two inputs (uncertain edges/pdag object)
####into tidy df of undirected edges.

#parse "A -- B"/"A--B" strings into a data.frame of unordered pairs
.dagassist_parse_uncertain <- function(uncertain_edges) {
  empty <- data.frame(v = character(0), w = character(0), stringsAsFactors = FALSE)
  if (is.null(uncertain_edges) || !length(uncertain_edges)) return(empty)
  rows <- lapply(uncertain_edges, function(s) {
    s <- gsub("<->|->|<-", "--", s) # tolerate arrows; treat as uncertain
    p <- trimws(strsplit(s, "--", fixed = TRUE)[[1]])
    p <- p[nzchar(p)]
    #fail helpfully if can't parse 
    if (length(p) != 2L)
      stop(sprintf("Could not parse uncertain edge '%s'. Use 'A -- B'.", s), call. = FALSE)
    data.frame(v = p[1], w = p[2], stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#pull undirected ('--') edges out of a dagitty pdag object.
.dagassist_pdag_undirected <- function(pdag) {
  empty <- data.frame(v = character(0), w = character(0), stringsAsFactors = FALSE)
  if (is.null(pdag)) return(empty)
  if (!inherits(pdag, "dagitty"))
    stop("`pdag` must be a dagitty object, e.g. dagitty::dagitty('pdag { X -> Y; A -- B }').",
         call. = FALSE)
  E <- dagitty::edges(pdag)
  und <- E[E$e == "--", c("v", "w"), drop = FALSE]
  data.frame(v = as.character(und$v), w = as.character(und$w), stringsAsFactors = FALSE)
}

#keep structural (non-uncertain) edges verbatim 
.dagassist_structural_edges <- function(dag) {
  E <- dagitty::edges(dag)
  E[E$e %in% c("->", "<->"), c("v", "w", "e"), drop = FALSE]
}

#derive a directed root DAG from a pdag (i.e., drop '--' and use when only `pdag` given)
.dagassist_pdag_to_dag <- function(pdag) {
  E <- dagitty::edges(pdag)
  dir <- E[E$e %in% c("->", "<->"), , drop = FALSE]
  es <- if (nrow(dir)) sprintf("%s %s %s", dir$v, dir$e, dir$w) else character(0)
  d <- dagitty::dagitty(paste0("dag { ", paste(c(names(pdag), es), collapse = "; "), " }"))
  dagitty::exposures(d) <- dagitty::exposures(pdag)
  dagitty::outcomes(d)  <- dagitty::outcomes(pdag)
  d
}

####algorithm to enumerate the 'worlds'. for k uncertain edges, build the 2^k 
####orientation combinations, keep the acyclic ones, and tag each by its orientation.

#unordered pair key (so A--B and B--A match)
.dagassist_pair_key <- function(v, w) paste(sort(c(v, w)), collapse = "\u0001")

#all acyclic DAGs consistent with dag's fixed edges + every orientation of the
#uncertain pairs. returns a named list of dagitty DAGs (names = orientation tag).
.dagassist_build_worlds <- function(dag, uncertain, exposure, outcome,
                                    max_uncertain = 10L) {
  nodes <- names(dag)
  miss <- setdiff(unique(c(uncertain$v, uncertain$w)), nodes)
  if (length(miss))
    stop("Uncertain edge(s) reference nodes not in the DAG: ",
         paste(miss, collapse = ", "), ". Add them to the DAG first.", call. = FALSE)
  
  k <- nrow(uncertain)
  if (k == 0L) return(list())
  if (k > max_uncertain)
    stop(sprintf("Too many uncertain edges (%d -> 2^%d worlds). Raise `max_uncertain` to override.",
                 k, k), call. = FALSE)
  
  #fixed edges = structural edges minus any that coincide with an uncertain pair
  struct <- .dagassist_structural_edges(dag)
  unc_keys <- mapply(.dagassist_pair_key, uncertain$v, uncertain$w)
  if (nrow(struct)) {
    struct_keys <- mapply(.dagassist_pair_key, struct$v, struct$w)
    struct <- struct[!struct_keys %in% unc_keys, , drop = FALSE]
  }
  fixed_str <- if (nrow(struct)) sprintf("%s %s %s", struct$v, struct$e, struct$w) else character(0)
  
  grid <- expand.grid(rep(list(c(0L, 1L)), k), KEEP.OUT.ATTRS = FALSE)
  worlds <- list(); tags <- character(0)
  for (i in seq_len(nrow(grid))) {
    es <- fixed_str; tag <- character(0)
    for (j in seq_len(k)) {
      a <- uncertain$v[j]; b <- uncertain$w[j]
      if (grid[i, j] == 0L) { es <- c(es, sprintf("%s -> %s", a, b)); tag <- c(tag, sprintf("%s\u2192%s", a, b)) }
      else { es <- c(es, sprintf("%s -> %s", b, a)); tag <- c(tag, sprintf("%s\u2192%s", b, a)) }
    }
    spec <- paste0("dag { ", paste(c(nodes, es), collapse = "; "), " }")
    d <- tryCatch(dagitty::dagitty(spec), error = function(e) NULL)
    if (is.null(d) || !isTRUE(dagitty::isAcyclic(d))) next   # drop cyclic orientations
    dagitty::exposures(d) <- exposure
    dagitty::outcomes(d)  <- outcome
    worlds[[length(worlds) + 1L]] <- d
    tags <- c(tags, paste(tag, collapse = ", "))
  }
  names(worlds) <- tags
  worlds
}

####diff engine + flagging rules 
####when is something a problem? sets 4 rules:
####1. minimal/canonical set changed
####2. role ambiguity (i.e., node occupies multiple distinct primary roles across worlds)
####3. critical ambiguity---node crosses good <-> bad threshold (e.g., Z -> M / M -> Z)
####4. reestimation recommended when minimal change OR canonical change OR critical ambiguity

#set of bad controls for total-effect estimation (never-adjust)
.DAGASSIST_BAD_ROLES <- c("mediator", "collider", "dOut", "Dmediator", "Dcollider")

#canonical string for a family of sets (order-independent)
.dagassist_family_key <- function(sets) {
  if (!length(sets)) return("<none>")
  inner <- vapply(sets, function(s) paste(sort(s), collapse = ","), character(1))
  paste(sort(inner), collapse = " | ")
}

##main engine
.dagassist_pdag_robustness <- function(dag, exposure, outcome, uncertain,
                                       formula = NULL, max_uncertain = 10L) {
  worlds <- .dagassist_build_worlds(dag, uncertain, exposure, outcome, max_uncertain)
  
  #baseline (root) computed on the dag as given
  root_roles <- classify_nodes(dag, exposure, outcome)
  root_min   <- .minimal_sets_all(dag, exposure, outcome)
  root_canon <- .pick_canonical_controls(dag, exposure, outcome)
  
  per <- lapply(worlds, function(w) list(
    roles = classify_nodes(w, exposure, outcome),
    min = .minimal_sets_all(w, exposure, outcome),
    canon = .pick_canonical_controls(w, exposure, outcome)
  ))
  
  #rule 1: minimal / canonical changed across root + worlds?
  min_keys   <- c(.dagassist_family_key(root_min),
                  vapply(per, function(p) .dagassist_family_key(p$min), character(1)))
  canon_keys <- c(.dagassist_family_key(list(root_canon)),
                  vapply(per, function(p) .dagassist_family_key(list(p$canon)), character(1)))
  minimal_changed   <- length(unique(min_keys))   > 1L
  canonical_changed <- length(unique(canon_keys)) > 1L
  
  #rules 2 & 3: per-node role ambiguity, flag good/bad crossings
  role_of <- function(rdf, v) { r <- rdf$role[rdf$variable == v]; if (length(r)) r[1] else NA_character_ }
  rows <- list()
  for (v in root_roles$variable) {
    roles_v <- c(role_of(root_roles, v), vapply(per, function(p) role_of(p$roles, v), character(1)))
    roles_v <- unique(roles_v[!is.na(roles_v)])
    if (length(roles_v) > 1L) {
      crosses <- any(roles_v %in% .DAGASSIST_BAD_ROLES) && any(!roles_v %in% .DAGASSIST_BAD_ROLES)
      rows[[length(rows) + 1L]] <- data.frame(
        variable = v,
        root_role = role_of(root_roles, v),
        roles_across_worlds = paste(sort(roles_v), collapse = " / "),
        critical = crosses,
        stringsAsFactors = FALSE
      )
    }
  }
  role_changes <- if (length(rows)) do.call(rbind, rows) else
    data.frame(variable = character(0), root_role = character(0),
               roles_across_worlds = character(0), critical = logical(0),
               stringsAsFactors = FALSE)
  
  #rule 4: re-estimation recommended?
  formula_vars <- if (!is.null(formula)) .rhs_terms_safe(formula) else character(0)
  critical_in_formula <- nrow(role_changes) > 0L &&
    any(role_changes$critical & role_changes$variable %in% formula_vars)
  reestimate <- minimal_changed || canonical_changed || critical_in_formula
  
  structure(list(
    n_uncertain = nrow(uncertain),
    n_worlds = length(worlds),
    uncertain_edges = if (nrow(uncertain)) sprintf("%s -- %s", uncertain$v, uncertain$w) else character(0),
    minimal_changed = minimal_changed,
    canonical_changed = canonical_changed,
    role_changes = role_changes,
    reestimate = reestimate,
    root = list(roles = root_roles, min = root_min, canon = root_canon),
    worlds = per
  ), class = "DAGassist_pdag_summary")
}

################################################################################
########################## EXPORTED FUNCTIONS ##################################
################################################################################
#printer and main function

#' Print a PDAG robustness summary
#' @param x A `DAGassist_pdag_summary` from [pdag_robustness()].
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
print.DAGassist_pdag_summary <- function(x, ...) {
  yn <- function(b) if (isTRUE(b)) "yes" else "no"
  cat(clr_bold("\nPDAG robustness summary:"), "\n", sep = "")
  cat("- uncertain edges specified: ", x$n_uncertain, "\n", sep = "")
  cat("- worlds evaluated (acyclic orientations): ", x$n_worlds, "\n", sep = "")
  cat("- minimal adjustment set changed: ",   yn(x$minimal_changed),   "\n", sep = "")
  cat("- canonical adjustment set changed: ", yn(x$canonical_changed), "\n", sep = "")
  if (nrow(x$role_changes)) {
    for (i in seq_len(nrow(x$role_changes))) {
      rc  <- x$role_changes[i, ]
      tag <- if (isTRUE(rc$critical)) clr_red(" (good/bad control flip)") else ""
      cat("- covariate role changed: ", rc$root_role, " -> ambiguous (",
          rc$roles_across_worlds, ") for ", rc$variable, tag, "\n", sep = "")
    }
  } else {
    cat("- covariate role classifications changed: none\n")
  }
  cat("- re-estimation recommended: ",
      if (isTRUE(x$reestimate)) clr_yellow("yes") else clr_green("no"), "\n", sep = "")
  invisible(x)
}

#' Diagnose adjustment-set and role robustness to uncertain edge directions
#'
#' Given a DAG plus one or more edges whose direction is uncertain (a PDAG),
#' enumerate every acyclic orientation and report whether the minimal/canonical
#' adjustment sets or covariate roles change -- i.e., whether your estimand is
#' robust to that structural uncertainty.
#'
#' @param dag A `dagitty` DAG (the "root" model).
#' @param exposure,outcome Optional; inferred from the DAG when omitted.
#' @param uncertain_edges Character vector like `c("A -- B")` naming edges whose
#'   direction is unknown.
#' @param pdag Optional `dagitty` PDAG; its `--` edges are treated as uncertain.
#' @param formula Optional model formula; used to decide whether an ambiguous
#'   covariate is actually in your specification (affects re-estimation advice).
#' @param max_uncertain Integer guard on the number of uncertain edges
#'   (default 10 -> up to 1024 worlds).
#' @return A `DAGassist_pdag_summary` object (printed as a bullet summary).
#' @examplesIf requireNamespace("dagitty", quietly = TRUE)
#' g <- dagitty::dagitty("dag { Z->X; X->Y; Z->Y; A->B; B->Y }")
#' dagitty::exposures(g) <- "X"; dagitty::outcomes(g) <- "Y"
#' pdag_robustness(g, uncertain_edges = "A -- B")
#' @export
pdag_robustness <- function(dag, exposure, outcome,
                            uncertain_edges = NULL, pdag = NULL,
                            formula = NULL, max_uncertain = 10L) {
  # Accept a plain formula OR an engine call like lm(y ~ x, data = d), and pull
  # out ONLY the formula -- never fit the model. Mirrors DAGassist(); avoids
  # deparsing a fitted model object (pathologically slow via .split_top_level()).
  spec_expr <- substitute(formula)
  fml <- NULL
  if (!is.null(spec_expr)) {
    if (is.call(spec_expr) && !identical(spec_expr[[1L]], as.name("~"))) {
      fml <- tryCatch(
        .extract_from_engine_call(spec_expr, eval_env = parent.frame())$formula,
        error = function(e) NULL
      )
    }
    if (is.null(fml) && !is.null(formula)) {            # plain formula, or fitted model
      fml <- if (inherits(formula, "formula")) formula
      else tryCatch(stats::formula(formula), error = function(e) NULL)
    }
  }
  
  if (missing(dag) || is.null(dag)) {
    if (is.null(pdag)) stop("Supply a `dag` (and `uncertain_edges`) or a `pdag`.", call. = FALSE)
    dag <- .dagassist_pdag_to_dag(pdag)
  }
  xy  <- .infer_xy(dag, if (missing(exposure)) NULL else exposure,
                   if (missing(outcome))  NULL else outcome)
  und <- rbind(.dagassist_parse_uncertain(uncertain_edges),
               .dagassist_pdag_undirected(pdag))
  if (!nrow(und)) stop("Supply `uncertain_edges` or a `pdag` containing '--' edges.", call. = FALSE)
  .dagassist_pdag_robustness(dag, xy$exposure, xy$outcome, und,
                             formula = fml, max_uncertain = max_uncertain)
}