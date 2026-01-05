# build a unified, named list of models for all outputs.
# (this is important. each of the out paths use .build_named_mods)
##IN: `report` the list returned inside DAGassist(), which contains 
#report$models$original -- fitted base model
#report$models$minimal -- fitted minimal model
#report$models$minimal_list -- list of all minimal fits
#report$models$canonical -- fitted canonical model
#report$formulas$original -- formula for original
#report$formulas$canonical--  formula for Canonical
#report$controls_minimal--vector of minimal controls 
#
##OUT:
#named list of models with nice labels
# e.g. ("Original", "Minimal 1", "Minimal 2", "Canonical")
#
##NOTE:
#always includes original
#if multiple minimal models exist labels them "Minimal (number)"
#if there is a single minimal specification, include it as "Minimal 1".
#include distinct canonical column only if  different from "Original" to avoid
# duplicate columns downstream. the column will be lableled canonical, so no interp issues
#presence of Canonical is decided via .same_formula().
#downstream exporters rely on these names as table headers.
#updated to add compatability with sequential g estimation via DirectEffects
.build_named_mods <- function(report) {
  
  mods <- report$models
  
  # Base models
  mods_full <- list("Original" = mods$original)
  
  if (!is.null(mods$bivariate)) {
    mods_full[["Bivariate"]] <- mods$bivariate
  }
  
  if (length(mods$minimal_list)) {
    for (i in seq_along(mods$minimal_list)) {
      mods_full[[sprintf("Minimal %d", i)]] <- mods$minimal_list[[i]]
    }
  } else {
    mods_full[["Minimal 1"]] <- mods$minimal
  }
  
  mods_full[["Canonical"]] <- mods$canonical
  
  if (!is.null(mods$canonical_excl)) {
    cex <- mods$canonical_excl
    if (is.list(cex)) {
      for (nm in names(cex)) {
        lbl <- switch(
          nm,
          nct = "Canon. (-NCT)",
          nco = "Canon. (-NCO)",
          paste0("Canonical (", nm, ")")
        )
        mods_full[[lbl]] <- cex[[nm]]
      }
    } else {
      # old single-model behavior
      exc <- report$settings$exclude
      if (!is.null(exc)) {
        if (identical(exc, "nct")) lbl <- "Canon. (-NCT)"
        if (identical(exc, "nco")) lbl <- "Canon. (-NCO)"
        mods_full[[lbl]] <- cex
      }
    }
  }
  
  # Add ATE/ATT weighted + ACDE via dispatcher (no-op if RAW)
  mods_full <- .dagassist_add_estimand_models(report, mods_full)
  
  mods_full
}

# Build a dagitty subgraph restricted to `keep` nodes.
# Keeps only edges with both endpoints in `keep`.
.restrict_dag_to <- function(dag, keep) {
  keep <- intersect(keep, names(dag))
  # Build edges via parents() to avoid depending on dagitty::edges format
  edge_str <- character(0)
  for (w in keep) {
    ps <- intersect(dagitty::parents(dag, w), keep)
    if (length(ps)) edge_str <- c(edge_str, paste(ps, "->", w))
  }
  if (length(edge_str)) {
    spec <- paste0("dag { ", paste(edge_str, collapse = "; "), " }")
  } else {
    # No edges; declare a node-only DAG
    spec <- paste0("dag { ", paste(keep, collapse = "; "), " }")
  }
  d2 <- dagitty::dagitty(spec)
  # carry exposure/outcome tags if still present
  try(dagitty::exposures(d2) <- intersect(dagitty::exposures(dag), keep), silent = TRUE)
  try(dagitty::outcomes(d2)  <- intersect(dagitty::outcomes(dag),  keep), silent = TRUE)
  d2
}

###grab .build_named_mods labels and build df
##IN: report
##OUT: df with cols `Model` and `Formula`
##NOTES from .build_named_mods apply here too
.build_models_df <- function(report) {
  
  f <- report$formulas
  
  model_formulas <- list("Original" = f$original)
  
  if (!is.null(f$bivariate)) {
    model_formulas[["Bivariate"]] <- f$bivariate
  }
  
  if (length(f$minimal_list)) {
    for (i in seq_along(f$minimal_list)) {
      model_formulas[[sprintf("Minimal %d", i)]] <- f$minimal_list[[i]]
    }
  } else {
    model_formulas[["Minimal 1"]] <- f$minimal
  }
  
  model_formulas[["Canonical"]] <- f$canonical
  
  if (!is.null(f$canonical_excl)) {
    cex <- f$canonical_excl
    if (is.list(cex)) {
      for (nm in names(cex)) {
        lbl <- switch(
          nm,
          nct = "Canon. (-NCT)",
          nco = "Canon. (-NCO)",
          paste0("Canonical (", nm, ")")
        )
        model_formulas[[lbl]] <- cex[[nm]]
      }
    } else {
      exc <- report$settings$exclude
      if (!is.null(exc)) {
        if (identical(exc, "nct")) lbl <- "Canon. (-NCT)"
        if (identical(exc, "nco")) lbl <- "Canon. (-NCO)"
        model_formulas[[lbl]] <- cex
      }
    }
  }
  
  # Add derived formula rows for requested estimands (ATE/ATT/ACDE)
  ests <- .dagassist_normalize_estimand(report$settings$estimand)
  
  if ("ATE" %in% ests) {
    wlab <- .dagassist_model_name_labels("ATE")
    for (nm in names(model_formulas)) {
      model_formulas[[paste0(nm, " ", wlab)]] <- model_formulas[[nm]]
    }
  }
  
  if ("ATT" %in% ests) {
    wlab <- .dagassist_model_name_labels("ATT")
    for (nm in names(model_formulas)) {
      model_formulas[[paste0(nm, " ", wlab)]] <- model_formulas[[nm]]
    }
  }
  
  if ("ACDE" %in% ests) {
    alab <- .dagassist_model_name_labels("ACDE")
    for (nm in names(model_formulas)) {
      nm_acde <- paste0(nm, " ", alab)
      # build sequential_g formula from base model formula
      model_formulas[[nm_acde]] <- .dagassist_formula_for_model_name(report, nm_acde)
    }
  }
  
  models_df <- data.frame(
    model_name = names(model_formulas),
    formula = vapply(model_formulas, function(ff) paste(deparse(ff), collapse=" "), character(1)),
    type = rep("comparison", length(model_formulas)),
    stringsAsFactors = FALSE
  )
  
  list(models_df = models_df, model_formulas = model_formulas)
}


### infer x and y from the call, so the user does not have to make an  
### "engine" call 
##IN: dag -- dagitty object
##OUT: list(exposure=<chr>,outcome=<chr>)
##NOTE:
#if exposure/outcome is missing, infer from dagitty object
#if one exposure/outcome is found, use it
#if multiple, tell user to pick one
.infer_xy <- function(dag, exposure, outcome) {
  # If either exposure or outcome is missing/empty, infer from dagitty
  # exposure: accept vector
  if (missing(exposure) || is.null(exposure) ||
      (is.character(exposure) && !length(exposure)) ||
      !nzchar(paste(exposure, collapse = ""))) {
    
    ex <- tryCatch(dagitty::exposures(dag), error = function(e) character(0))
    if (length(ex) == 0L) {
      stop("Please supply `exposure=`; DAG has 0 exposures.", call. = FALSE)
    }
    # <- this is the key change: just take all of them
    exposure <- ex
  } else {
    exposure <- as.character(exposure)
  }
  
  # same approach for outcome
  if (missing(outcome) || is.null(outcome) || !nzchar(outcome)) {
    out <- tryCatch(dagitty::outcomes(dag), error = function(e) character(0))
    if (length(out) == 1) {
      outcome <- out
    } else {
      stop(
        "Please supply `outcome=`; DAG has ", length(out), " outcome(s).",
        call. = FALSE
      )
    }
  }
  # Return normalized names for downstream use
  list(exposure = exposure, outcome = outcome)
}

###figure out if user wrote `~` directly or via parsed code
##examples:
#.is_formula_expr(quote(Y ~ X)) #TRUE
#.is_formula_expr(~ Y + X) #TRUE
#.is_formula_expr(quote(lm(Y ~ X))) #FALSE --part of lm call
#.is_formula_expr(quote("Y ~ X")) #FALSE --string, not call)
.is_formula_expr <- function(expr) {
  is.call(expr) && identical(expr[[1]], as.name("~"))
}

# Resolve an argument that might reference a data column into the actual vector.
# Accepts: character "col", bare symbol col, or one-sided formula ~ col
# Returns: data[[col]] if found; otherwise NULL (caller can decide what to do)
.resolve_in_data_arg <- function(expr, data) {
  # character: "col"
  if (is.character(expr) && length(expr) == 1L && expr %in% names(data)) {
    return(data[[expr]])
  }
  # bare name: col
  if (is.name(expr)) {
    nm <- as.character(expr)
    if (nm %in% names(data)) return(data[[nm]])
  }
  # one-sided formula: ~ col
  if (inherits(expr, "formula") && length(expr) == 2L) {
    vars <- all.vars(expr)
    if (length(vars) == 1L && vars %in% names(data)) {
      return(data[[vars]])
    }
  }
  # not resolvable
  NULL
}

## Extract engine, formula, data, and extra args from an unevaluated call like
##   feols(Y ~ X + Z | A, data = df, cluster = ~id)
## to avoid separate, explicit "formula", "data", "engine", and "engine_args"
## arguments. 
.extract_from_engine_call <- function(expr, eval_env = parent.frame()) {
  stopifnot(is.call(expr))
  fn_name <- as.character(expr[[1]])
  # find the function; respects namespaces if user wrote fixest::feols
  fn <- eval(expr[[1]], envir = eval_env)
  
  # turn call into a named list of arguments (some may be unnamed/positional)
  args_list <- as.list(expr)[-1]
  arg_names <- names(args_list)
  if (is.null(arg_names)) arg_names <- rep("", length(args_list))
  
  # locate the formula: prefer 'fml' or 'formula', else first positional
  f_idx <- which(arg_names %in% c("fml", "formula"))[1]
  if (is.na(f_idx)) f_idx <- 1L
  f_expr <- args_list[[f_idx]]
  fml   <- eval(f_expr, envir = eval_env)
  
  # locate data: prefer 'data', else next positional after fml
  d_idx <- which(arg_names == "data")[1]
  if (is.na(d_idx)) {
    # next positional after f_idx
    pos <- seq_along(args_list)
    pos <- pos[arg_names == ""]
    pos <- pos[pos > f_idx]
    if (length(pos)) d_idx <- pos[1] else d_idx <- NA_integer_
  }
  if (is.na(d_idx)) {
    stop("Could not find `data=` in the engine call. Please supply data.", call. = FALSE)
  }
  data_expr <- args_list[[d_idx]]
  data_obj  <- eval(data_expr, envir = eval_env)
  # everything else becomes engine_args
  keep  <- setdiff(seq_along(args_list), c(f_idx, d_idx))
  extra <- args_list[keep]
  ##handles cluster arguments
  if (length(extra)) {
    nms <- names(extra)
    engine_args <- vector("list", length(extra))

    for (i in seq_along(extra)) {
      nm <- nms[i]
      ex <- extra[[i]]

      # Try normal evaluation first
      val <- try(eval(ex, envir = eval_env), silent = TRUE)

      # If it failed OR it's a cluster-like arg given as a "column pointer",
      # resolve from `data_obj` when possible
      is_try_err <- inherits(val, "try-error")
      is_cluster_arg <- nzchar(nm) && nm %in% c("clusters", "cluster")

      if (is_try_err || is_cluster_arg) {
        # If normal eval failed OR the result is a *single* character that names a column,
        # or the original expr is a symbol/formula naming a column, swap in data[[...]]
        candidate <- if (!is_try_err) {
          # eval succeeded: maybe user passed "cat_b"
          if (is.character(val) && length(val) == 1L) .resolve_in_data_arg(val, data_obj) else NULL
        } else {
          # eval failed: try resolving the *expression* inside `data`
          .resolve_in_data_arg(ex, data_obj)
        }

        if (!is.null(candidate)) {
          val <- candidate
        } else if (is_try_err) {
          # give the original, readable error if we couldn't resolve
          stop(as.character(attr(val, "condition")), call. = FALSE)
        }
      }

      engine_args[[i]] <- val
    }
    names(engine_args) <- nms
  } else {
    engine_args <- list()
  }
  
  list(engine = fn, formula = fml, data = data_obj, engine_args = engine_args)
}

# split on a single-character separator only when we're at top level
# this helps with RE | 
.split_top_level <- function(s, sep = "|") {
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  out <- character(); buf <- character()
  depth <- 0L
  in_quote <- FALSE; qchar <- ""
  n <- length(chars)
  
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    
    # track quotes (skip escaped quotes)
    if (!in_quote && (ch == "'" || ch == '"')) {
      in_quote <- TRUE; qchar <- ch
      buf <- c(buf, ch); i <- i + 1L; next
    } else if (in_quote && ch == qchar) {
      buf <- c(buf, ch); in_quote <- FALSE; qchar <- ""
      i <- i + 1L; next
    }
    
    if (!in_quote) {
      if (ch == "(") { depth <- depth + 1L; buf <- c(buf, ch); i <- i + 1L; next }
      if (ch == ")") { depth <- max(0L, depth - 1L); buf <- c(buf, ch); i <- i + 1L; next }
      
      # split only on top-level sep
      if (ch == sep && depth == 0L) {
        out <- c(out, trimws(paste(buf, collapse = "")))
        buf <- character()
        i <- i + 1L
        next
      }
    }
    
    buf <- c(buf, ch)
    i <- i + 1L
  }
  
  c(out, trimws(paste(buf, collapse = "")))
}

# Extract term labels from the k-th top-level '|' block of a formula.
# k = 1 is the main (pre-|) RHS; k = 2 is the first tail block (FE in fixest/felm),
# k = 3 is the next block, etc.
.dagassist_bar_block_terms <- function(fml, k = 2L) {
  if (!inherits(fml, "formula")) return(character(0))
  
  s <- paste(deparse(fml, width.cutoff = 500L), collapse = " ")
  parts <- .split_top_level(s, sep = "|")
  if (length(parts) < k) return(character(0))
  
  txt <- trimws(parts[[k]])
  if (!nzchar(txt)) return(character(0))
  
  # Build a RHS-only formula and pull term labels
  rhs_fml <- stats::as.formula(paste0("~", txt), env = environment(fml))
  
  # If the block contains no variables (e.g., 1/0/TRUE/FALSE), treat as placeholder
  expr <- rhs_fml[[2L]]
  if (!length(all.vars(expr, functions = FALSE))) return(character(0))
  
  unique(attr(stats::terms(rhs_fml), "term.labels"))
}

# Factorize only bare symbols; leave complex terms untouched (i(), interactions, etc.)
.dagassist_factorize_plain_terms <- function(terms) {
  if (!length(terms)) return(character(0))
  is_bare <- grepl("^[.A-Za-z][.A-Za-z0-9._]*$", terms)
  terms[is_bare] <- paste0("factor(", terms[is_bare], ")")
  terms
}

# Helper: collect any calls whose operator is '|' or '||' anywhere in the RHS.
# This is engine-agnostic and does not import lme4.
.collect_bar_calls <- function(expr, acc = list()) {
  if (is.call(expr)) {
    head <- as.character(expr[[1L]])
    if (head %in% c("|", "||")) {
      acc <- c(acc, list(expr))
    }
    # Recurse into all arguments
    for (i in seq_along(expr)) {
      acc <- .collect_bar_calls(expr[[i]], acc)
    }
  }
  acc
}

# Return FE / grouping variable *names* from common syntaxes:
#  (a) fixest/lfe tails: y ~ x | FE (+ optional more | blocks)
#  (b) lme4/nlme random-effects bars on RHS: (1 | FE), (x || FE), etc.
#
# We return *names* (e.g., "ID", "year"), not full terms.
.dagassist_extract_fe_vars <- function(fml) {
  if (!inherits(fml, "formula")) return(character(0))
  
  out <- character(0)
  
  # (a) tail block 2 for y ~ x | FE (fixest, felm, etc.)
  # This uses your existing .dagassist_bar_block_terms().
  fe_terms <- .dagassist_bar_block_terms(fml, k = 2L)
  if (length(fe_terms)) {
    # Convert any complex terms into underlying variable names.
    # Example: "i(ID, ref=1)" -> "ID"
    out <- c(out, unique(unlist(lapply(fe_terms, function(tt) {
      expr <- tryCatch(parse(text = tt)[[1]], error = function(e) NULL)
      if (is.null(expr)) return(character(0))
      all.vars(expr, functions = FALSE)
    }))))
  }
  
  # (b) random-effects grouping variables like (1 | ID) or (x || ID)
  base <- .strip_fixest_parts(fml)$base
  rhs  <- tryCatch(base[[3L]], error = function(e) NULL)
  if (!is.null(rhs)) {
    bars <- .collect_bar_calls(rhs)
    if (length(bars)) {
      re_vars <- unique(unlist(lapply(bars, function(b) {
        # b is a call like `|`(lhs, group) or `||`(lhs, group)
        grp <- b[[3L]]
        all.vars(grp, functions = FALSE)
      })))
      out <- c(out, re_vars)
    }
  }
  
  # Drop constants if they ever leak in via parsing
  out <- setdiff(unique(out), c("TRUE", "FALSE", "T", "F"))
  out[nzchar(out)]
}

# Keep only term strings that reference >=1 real data column.
# Drops constants like TRUE/FALSE/1/0 and any term that doesn't resolve to data vars.
.dagassist_terms_must_use_data <- function(terms, data_names) {
  if (!length(terms)) return(character(0))
  terms <- unique(trimws(as.character(terms)))
  terms <- terms[nzchar(terms)]
  
  keep <- vapply(terms, function(tt) {
    expr <- tryCatch(parse(text = tt)[[1L]], error = function(e) NULL)
    if (is.null(expr)) return(FALSE)
    
    vars <- all.vars(expr, functions = FALSE)
    
    # constants like TRUE/FALSE typically yield vars==0 or vars=="TRUE"
    if (!length(vars)) return(FALSE)
    if (!all(vars %in% data_names)) return(FALSE)
    TRUE
  }, logical(1L))
  
  terms[keep]
}

# TRUE if x has no within-group variation (ignoring NAs)
.dagassist_is_constant_within <- function(x, g) {
  if (length(x) != length(g)) return(FALSE)
  sp <- split(x, g, drop = TRUE)
  all(vapply(sp, function(z) length(unique(z[!is.na(z)])) <= 1L, logical(1)))
}

# Drop any term whose *single underlying variable* is constant within any FE group.
# This catches time-invariant-within-ID covariates when unit FE are included, etc.
.dagassist_drop_terms_collinear_with_fe <- function(terms, data, fe_vars) {
  if (!length(terms) || !length(fe_vars) || is.null(data)) {
    return(list(keep = terms, dropped = character(0)))
  }
  
  data_names <- names(data)
  fe_vars <- intersect(unique(as.character(fe_vars)), data_names)
  if (!length(fe_vars)) return(list(keep = terms, dropped = character(0)))
  
  terms <- unique(trimws(as.character(terms)))
  terms <- terms[nzchar(terms)]
  terms <- setdiff(terms, c("0","1","TRUE","FALSE","T","F"))
  
  dropped <- character(0)
  keep <- character(0)
  
  for (tt in terms) {
    vars <- tryCatch(all.vars(parse(text = tt)[[1]], functions = FALSE),
                     error = function(e) character(0))
    
    # Only handle the simple/important case: terms that resolve to exactly one variable.
    # If it's a composite expression (e.g., log(x), x:z), do not attempt to drop.
    if (length(vars) != 1L) {
      keep <- c(keep, tt)
      next
    }
    
    v <- vars[1]
    # Don't drop FE terms themselves (factor(ID), ID, etc.)
    if (v %in% fe_vars || !(v %in% data_names)) {
      keep <- c(keep, tt)
      next
    }
    
    # If v is constant within ANY FE group, it is collinear with that FE.
    collinear <- FALSE
    for (fe in fe_vars) {
      if (.dagassist_is_constant_within(data[[v]], data[[fe]])) {
        collinear <- TRUE
        dropped <- c(dropped, tt)
        break
      }
    }
    
    if (!collinear) keep <- c(keep, tt)
  }
  
  list(keep = unique(keep), dropped = unique(dropped))
}

# strip fixest FE/IV parts AND preserve random-effect bars (| or ||) from the RHS
.strip_fixest_parts <- function(fml) {
  s <- paste(deparse(fml, width.cutoff = 500L), collapse = " ")
  
  # 1) Split only on top-level '|' (fixest FE/IV); your current splitter is correct.
  parts <- .split_top_level(s, sep = "|")
  base_text   <- parts[1]
  fixest_tail <- if (length(parts) >= 2) paste0(" | ", paste(parts[-1], collapse = " | ")) else ""
  
  # 2) Parse the base (pre-fixest) as a formula so we can read the RHS expression.
  base_fml <- stats::as.formula(base_text, env = environment(fml))
  
  # 3) Engine-agnostic capture of RE bars: collect any subcalls with head '|' or '||' on the RHS.
  re_tail <- ""
  rhs <- tryCatch(base_fml[[3L]], error = function(e) NULL)
  if (!is.null(rhs)) {
    bars <- .collect_bar_calls(rhs)
    if (length(bars)) {
      bar_txt <- vapply(
        bars,
        function(x) paste0("(", paste(deparse(x), collapse = " "), ")"),
        character(1)
      )
      # These are added as + ( ... ) terms on the RHS
      re_tail <- paste0(" + ", paste(bar_txt, collapse = " + "))
    }
  }
  
  # 4) Return base + combined tail: first the RE bars we found, then any fixest tail.
  list(base = base_fml, tail = paste0(re_tail, fixest_tail))
}

# Build a minimal formula that preserves any fixest '|' tail if present
.build_minimal_formula <- function(orig_fml, exposure, outcome, minimal_controls) {
  sp <- .strip_fixest_parts(orig_fml)
  # main RHS we control
  rhs <- paste(c(exposure, minimal_controls), collapse = " + ")
  # if no controls, ensure just exposure appears on RHS
  rhs <- if (nzchar(rhs)) rhs else exposure
  if (nzchar(sp$tail)) {
    # Recompose string then parse
    f_str <- paste0(outcome, " ~ ", rhs, sp$tail)
    stats::as.formula(f_str, env = environment(orig_fml))
  } else {
    # simple case
    update_to_controls(exposure, outcome, minimal_controls)
  }
}

## Pick a deterministic canonical adjustment set
# hoose the set with the fewest controls and tie-break alphabetically for stability
# will need to add multiple-set functionality eventually
###NOTE:there should only ever be one canonical set, by definition. this is 
###just a belt+suspenders guardrail
.pick_canonical_controls <- function(dag, exposure, outcome) {
  sets <- tryCatch(
    dagitty::adjustmentSets(
      dag, exposure = exposure, outcome = outcome, type = "canonical"
      ),
    error = function(e) NULL
  )
  if (is.null(sets) || length(sets) == 0) return(character(0))
  
  # Normalize to sorted character vectors and defensively drop X/Y if present to be safe
  # accomodate full vector; don't force down to one 
  sets <- lapply(sets, function(s) {
    drop_me <- c(as.character(exposure), outcome)
    sort(setdiff(as.character(s), drop_me))
  })
  
  #early exit if one canonical set
  if (length(sets) == 1L) return(sets[[1L]])
  
  #fallback if multiple canonical sets are ever returned
  lens       <- vapply(sets, length, integer(1))
  candidates <- sets[lens == min(lens)]
  keys       <- vapply(candidates, function(s) paste(s, collapse = "|"), character(1))
  candidates[[order(keys)[1]]]
}
###helpers to enable intercept and factor row suppression
.escape_regex <- function(x) gsub("([\\W])", "\\\\\\1", x, perl = TRUE)

.build_coef_omit <- function(data, omit_intercept = FALSE, omit_factors = FALSE) {
  pats <- character(0)
  
  if (isTRUE(omit_intercept)) {
    #handles "(Intercept)" and "Intercept"
    pats <- c(pats, "^\\(Intercept\\)$|^Intercept$")
  }
  
  if (isTRUE(omit_factors) && !is.null(data)) {
    fac_names <- names(Filter(function(x) is.factor(x) || is.character(x), data))
    if (length(fac_names)) {
      #Robust to fixest/lm/glm/lme4 naming: "var::level", "varlevel", "var:level"
      #match any coefficient term beginning with one of the factor vars
      #followed by typical separators or word boundary
      fac_escaped <- vapply(fac_names, .escape_regex, "", USE.NAMES = FALSE)
      alt <- paste(fac_escaped, collapse = "|")
      #match all factor indicators
      fac_pat <- paste0("^((?:factor|as\\.factor)\\s*\\()?(", alt, ")(\\))?(::|:|=|\\b).*$")
      pats <- c(pats, fac_pat)
      # Also omit *explicit* factor()/as.factor() expansions even when the underlying
      # data column is numeric/integer (common for FE IDs and years).
      # Matches: "factor(ID)1354", "as.factor(year)2010", etc.
      pats <- c(pats, "^\\s*(?:factor|as\\.factor)\\([^\\)]+\\).*")
    }
  }
  
  if (length(pats)) paste(pats, collapse = "|") else NULL
}

###Build a formula from control set, preserving any fixest | tail, as in minimal
##IN
#orig_fml -- user's original formula with optional tail
#exposure -- exposure char
#outcome -- outcome char
#controls -- char vector of controls to keep on RHS
##NOTES
#reconstructs RHS as "exposure + controls"
#reattaches orig formaul tail verbatim
#otherwise, fall back to helper "update_to_controls"
#when controls is empty, rhs is just exposure
#build new formula as orig_fml
.build_formula_with_controls <- function(orig_fml, exposure, outcome, controls) {
  #split orig formula into base LHS and tail
  sp  <- .strip_fixest_parts(orig_fml)
  #compose main RHS--exposure+controls
  rhs <- paste(c(exposure, controls), collapse = " + ")
  #if RHS is empty, exposure fallback
  rhs <- if (nzchar(rhs)) rhs else exposure
  
  if (nzchar(sp$tail)) {
    #reattach tail if present
    f_str <- paste0(outcome, " ~ ", rhs, sp$tail)
    stats::as.formula(f_str, env = environment(orig_fml))
  } else {
    #no tail: fallback to helper
    update_to_controls(exposure, outcome, controls)
  }
}

# Get RHS term labels from the pre-| part so fixest doesn't confuse terms
.rhs_terms_safe <- function(fml) {
  base <- .strip_fixest_parts(fml)$base
  attr(stats::terms(base), "term.labels")
}


###return ALL minimal adjustment sets as a list 
##IN
#dag--dagitty object
#exposure--exposure var
#outcome--outcome var
##OUT
#list(<chr vec set 1>, <chr vec set 2>, ...); empty if none or error
##NOTE
#calls dagitty to get all min sets
#converts each set to sorted char vector
#removes exposure and outcome if present as belt+suspender guardrail
.minimal_sets_all <- function(dag, exposure, outcome) {
  sets <- tryCatch(
    dagitty::adjustmentSets(
      dag, 
      exposure = exposure,
      outcome = outcome,
      type = "minimal"
      ),
    error = function(e) NULL
  )
  if (is.null(sets) || length(sets) == 0) return(list())
  #normalize as char, drop x/y, sort
  # accomodate vector; don't chop down to single treatment
  lapply(sets, function(s) {
    drop_me <- c(as.character(exposure), outcome)
    sort(setdiff(as.character(s), drop_me))
  })
}

# detect composite DAG conditions from the roles table
# this detects m bias and butterfly bias
.detect_dag_conditions <- function(roles, dag, exposure, outcome) {
  res <- list(m_bias = FALSE, butterfly_bias = FALSE)
  
  if (is.null(dag) || is.null(exposure) || is.null(outcome)) {
    return(res)
  }
  
  # build ancestor sets
  ancX <- unique(unlist(lapply(exposure, function(x) dagitty::ancestors(dag, x))))
  ancY <- dagitty::ancestors(dag, outcome)
  descY <- setdiff(dagitty::descendants(dag, outcome), outcome)
  
  # X-only and Y-only ancestors (the classic M-bias parents)
  x_only <- setdiff(ancX, c(ancY, exposure, outcome))
  y_only <- setdiff(ancY, c(ancX, exposure, outcome, descY))
  
  colliders <- roles$variable[roles$is_collider]
  
  ## M-bias: collider with one parent from X-only and one from Y-only
  if (length(colliders)) {
    for (m in colliders) {
      parents_m <- dagitty::parents(dag, m)
      if (length(parents_m) < 2L) next
      if (any(parents_m %in% x_only) && any(parents_m %in% y_only)) {
        res$m_bias <- TRUE
        break
      }
    }
  }
  
  ## Butterfly bias: collider m that is ALSO an ancestor of both X and Y,
  ## and whose two parents feed into X and Y sides
  if (length(colliders)) {
    for (m in colliders) {
      parents_m <- dagitty::parents(dag, m)
      if (length(parents_m) != 2L) next
      a <- parents_m[1]; b <- parents_m[2]
      
      m_anc_X <- m %in% ancX
      m_anc_Y <- m %in% ancY
      a_anc_X <- a %in% ancX
      b_anc_Y <- b %in% ancY
      
      if (m_anc_X && m_anc_Y && a_anc_X && b_anc_Y) {
        res$butterfly_bias <- TRUE
        break
      }
    }
  }
  
  res
}

#pretty formatter for multiple adjustment sets: c("A","B") -> "{A, B}"
#similar to wrap, but replaces parinth
.format_set <- function(s) paste0("{", paste(s, collapse = ", "), "}")

##figure out if formulas text-identical
#used to avoid cases where canon and main are same to avoid sort issues
.same_formula <- function(f1, f2) {
  paste(deparse(f1), collapse = " ") == paste(deparse(f2), collapse = " ")
}

###safe fit wrapper so bad specs don't crash printing
##IN:
#engine -- like lm or feols
#fml -- formula to fit
#data -- valid df passed as data arg
#engine_args -- extra engine args
##NOTES:
#only catches errors. warn passes through
.safe_fit <- function(engine, fml, data, engine_args) {
  tryCatch(
    do.call(engine, c(list(fml, data), engine_args)),
    error = function(e) {
      structure(
        list(
          error   = conditionMessage(e),
          formula = fml
        ),
        class = "DAGassist_fit_error"
      )
    }
  )
}

#normalize labels into a named character vector: names = variable, values = label
.normalize_labels <- function(labels, vars) {
  if (missing(vars) || is.null(vars)) vars <- character(0)
  if (is.null(labels)) return(character(0))
  # already a named character vector
  if (is.character(labels) && length(names(labels))) return(labels)
  # flatten named list to named character
  if (is.list(labels) && length(names(labels))) {
    vals <- unlist(labels, use.names = TRUE, recursive = FALSE)
    vals <- vals[!vapply(vals, is.list, logical(1))]
    if (is.character(vals) && length(names(vals))) return(vals)
  }
  # df/list with cols variable and label
  if (is.list(labels) && all(c("variable","label") %in% names(labels))) {
    v <- as.character(labels$variable)
    l <- as.character(labels$label)
    names(l) <- v
    return(l)
  }
  # unnamed character vector in same order as vars
  if (is.character(labels) && !length(names(labels)) && length(vars) && length(labels) == length(vars)) {
    names(labels) <- as.character(vars)
    return(labels)
  }
  character(0)
}

#apply labels to roles df for downstream var name continuity
#does not rename col names
.apply_labels_to_roles_df <- function(roles, labmap) {
  if (is.null(roles) || !is.data.frame(roles)) return(roles)
  df <- roles
  disp <- as.character(df$variable)
  if (length(labmap)) {
    idx <- match(df$variable, names(labmap))
    repl <- !is.na(idx)
    disp[repl] <- unname(labmap[idx[repl]])
  }
  df$variable <- disp
  df
}
###print side-by-side comparison of models with graceful fallbacks
###THIS IS FOR CONSOLE. export has different helpers
##IN
#mods -- naed list of fitted model objects
###NOTES:
##priority order
#if modelsummary is installed, use msummary
#else if broom is installed, print simple tidy preview
#else print basic coef head
##OTHER NOTES
#omit gof rows to keep table small
#fit errors are printed as messages via DAGassist_fit_error
#returns invisibly
.print_model_comparison_list <- function(mods, coef_rename=NULL, coef_omit=NULL) {
  ##quick fail if there is an issue
  failed_idx <- vapply(mods, inherits, logical(1), what = "DAGassist_fit_error")
  failed <- mods[failed_idx]
  ok <- mods[!failed_idx]
  
  if (length(failed)) {
    cat("\nFit issues:\n")
    for (nm in names(failed)) cat(sprintf("  - %s: %s\n", nm, failed[[nm]]$error))
  }
  if (!length(ok)) {
    cat("\nAll model fits failed - no comparison table to print.\n")
    return(invisible(NULL))
  }
  ##preferred path: modelsummary
  if (requireNamespace("modelsummary", quietly = TRUE)) {
    args <- list(
      mods,
      stars = TRUE,
      output = "markdown",
      gof_map = NA
    )
    # only pass a VALID rename map (named char, length > 0)
    if (is.character(coef_rename) && length(coef_rename) && length(names(coef_rename))) {
      args$coef_rename <- coef_rename
    }#pass the coef omit arg to model
    if (!is.null(coef_omit)) {
      args$coef_omit <- coef_omit
    }
    tab <- do.call(modelsummary::msummary, args)
    cat("\nModel comparison:\n")
    #handle either vector or object
    if (is.character(tab)) {
      cat(paste(tab, collapse = "\n"))
    } else {
      print(tab)
    }
    return(invisible(NULL))
  }
  ##fallback:broom snapshot
  if (requireNamespace("broom", quietly = TRUE)) {
    cat("\nModel comparison (fallback; install {modelsummary} for a nicer table):\n")
    for (nm in names(mods)) {
      m <- mods[[nm]]
      #if fit failed, print error capture from earliers
      if (inherits(m, "DAGassist_fit_error")) {
        cat("\n", nm, " (fit error): ", m$error, "\n", sep = "")
        next
      }
      #try to tidy. if not supported, notify and move on to next fallback
      tt <- tryCatch(broom::tidy(m), error = function(e) NULL)
      if (is.null(tt)) { 
        cat("\n", nm, ": could not be tidied.\n", sep = "")
        next 
      }
      #print compact subset of typical columns
      cols <- intersect(c("term","estimate","std.error","statistic","p.value"), names(tt))
      #relabel if coef_rename is present
      if (length(coef_rename)) {
        idx <- match(tt$term, names(coef_rename))
        tt$term[!is.na(idx)] <- unname(coef_rename[idx[!is.na(idx)]])
      }
      cat("\n", nm, ":\n", sep = "") 
      #10 row head
      print(utils::head(tt[, cols, drop = FALSE], 10))
    }
    return(invisible(NULL))
  }
  ##last resort:coef() head
  cat("\nModel comparison (fallback; install {modelsummary} for a nicer table):\n")
  for (nm in names(mods)) {
    m <- mods[[nm]]
    cat("\n", nm, " (coef head):\n", sep = "")
    if (inherits(m, "DAGassist_fit_error")) { 
      cat("fit error: ", m$error, "\n", sep = "") 
      next 
    }
    print(utils::head(tryCatch(stats::coef(m), error = function(e) NULL)))
  }
  invisible(NULL)
}

#### helps find the exposure and outcome names 
#### used for the note on DAG-derived additions when imply = TRUE
###IN:
##roles -- dff produced by classify_nodes() with columns:
#variable -- char; node name
#role -- char
#is_exposure -- bool
#is_outcome -- bool
##value -- char scalar; expected vals "exposure" or "outcome"
###NOTES1
##if string role exists, prefer
##otherwise, fall back to logical flag columns is_exposure/is_outcome
##if not found, return NA_character_
###NOTES2
##only first match ret for downstream processing
##handles missing values
get_by_role <- function(roles, value) {
  #prefer explicit role column if exists
  if ("role" %in% names(roles)) {
    v <- roles$variable[roles$role == value]
    if (length(v)) return(v[1])
  }
  #fallbacks: bool flags
  if (identical(value, "exposure") &&
      !is.null(roles$is_exposure) &&
      any(roles$is_exposure)) {
    return(roles$variable[roles$is_exposure][1])
  }
  if (identical(value, "outcome") &&
      !is.null(roles$is_outcome) &&
      any(roles$is_outcome)) {
    return(roles$variable[roles$is_outcome][1])
  }
  # nothing found
  NA_character_
}

### color wrap helpers to prevent junking up knitr renders 
# accept ... and paste internally so user can pass multiple args. 

# TRUE when knitting
.is_knit <- function() isTRUE(getOption("knitr.in.progress"))

# Should we use ANSI colors right now?
.allow_ansi <- function() {
  # user option wins; default = colored only in interactive, non-knitr sessions
  opt <- getOption("DAGassist.color", default = (interactive() && !.is_knit()))
  opt &&
    Sys.getenv("NO_COLOR", "") == "" &&                  # standard override
    requireNamespace("crayon", quietly = TRUE) &&        # only if crayon is available
    crayon::has_color()                                  # and the terminal supports it
}

# Wrap a string with ANSI codes only if .allow_ansi() is TRUE
.clr_wrap <- function(prefix, suffix) {
  force(prefix); force(suffix)
  function(...) {
    s <- paste0(..., collapse = "")
    if (.allow_ansi()) paste0(prefix, s, suffix) else s
  }
}

#the pretty colors
clr_red    <- .clr_wrap("\033[31m", "\033[39m")
clr_green  <- .clr_wrap("\033[32m", "\033[39m")
clr_yellow <- .clr_wrap("\033[33m", "\033[39m")
clr_blue   <- .clr_wrap("\033[34m", "\033[39m")
clr_bold   <- .clr_wrap("\033[1m",  "\033[22m")