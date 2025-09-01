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
.build_named_mods <- function(report) {
  #start with base
  mods <- list("Original" = report$models$original)
  #number and label minimal sets
  if (length(report$models$minimal_list)) {
    for (i in seq_along(report$models$minimal_list)) {
      mods[[sprintf("Minimal %d", i)]] <- report$models$minimal_list[[i]]
    } #default minimal 1
  } else if (length(report$controls_minimal)) {
    mods[["Minimal 1"]] <- report$models$minimal
  }
  # separate canonical only if it differs from Original
  if (!.same_formula(report$formulas$canonical, report$formulas$original)) {
    mods[["Canonical"]] <- report$models$canonical
  }
  
  mods
}

###grab .build_named_mods labels and build df
##IN: report
##OUT: df with cols `Model` and `Formula`
##NOTES from .build_named_mods apply here too
.build_models_df <- function(report) {
  #label in specific order. subsequent table making has to work with this 
  labs <- c(
    "Original",
    if (length(report$formulas$minimal_list))
      paste0("Minimal ", seq_along(report$formulas$minimal_list))
    else if (length(report$controls_minimal))
      "Minimal 1"
    else character(0),
    if (!.same_formula(report$formulas$canonical, report$formulas$original))
      "Canonical"
    else character(0)
  )
  #deparse each formula to single line so it is easy to print
  forms <- c(
    paste(deparse(report$formulas$original), collapse = " "),
    if (length(report$formulas$minimal_list))
      vapply(
        report$formulas$minimal_list, 
        function(f) paste(deparse(f), collapse = " "), 
        character(1))
    else if (length(report$controls_minimal))
      paste(deparse(report$formulas$minimal), collapse = " ")
    else character(0),
    if (!.same_formula(report$formulas$canonical, report$formulas$original))
      paste(deparse(report$formulas$canonical), collapse = " ")
    else character(0)
  )
  #OUT
  data.frame(Model = labs, Formula = forms, stringsAsFactors = FALSE)
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
  if (missing(exposure) || is.null(exposure) || !nzchar(exposure)) {
    ex <- tryCatch(dagitty::exposures(dag), error = function(e) character(0))
    if (length(ex) == 1) {
      exposure <- ex
    } else {
      stop(
        "Please supply `exposure=`; DAG has ", length(ex), " exposure(s).",
        call. = FALSE
      )
    }
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
  keep <- setdiff(seq_along(args_list), c(f_idx, d_idx))
  extra <- args_list[keep]
  # evaluate each remaining arg now so do.call can use them
  if (length(extra)) {
    engine_args <- lapply(extra, function(e) eval(e, envir = eval_env))
    names(engine_args) <- names(extra)
  } else {
    engine_args <- list()
  }
  
  list(engine = fn, formula = fml, data = data_obj, engine_args = engine_args)
}

#strip fixest FE/IV parts for parsing RHS terms, and return
#base formula (pre-`|`) AND tail string OR "" if none
.strip_fixest_parts <- function(fml) {
  s <- paste(deparse(fml, width.cutoff = 500), collapse = " ")
  parts <- strsplit(s, "\\|")[[1]]
  base <- trimws(parts[1])
  tail <- if (length(parts) >= 2) paste0(" | ", paste(trimws(parts[-1]), collapse = " | ")) else ""
  base_fml <- stats::as.formula(base, env = environment(fml))
  list(base = base_fml, tail = tail)
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
  sets <- lapply(sets, function(s) sort(setdiff(as.character(s), c(exposure, outcome))))
  
  #early exit if one canonical set
  if (length(sets) == 1L) return(sets[[1L]])
  
  #fallback if multiple canonical sets are ever returned
  lens       <- vapply(sets, length, integer(1))
  candidates <- sets[lens == min(lens)]
  keys       <- vapply(candidates, function(s) paste(s, collapse = "|"), character(1))
  candidates[[order(keys)[1]]]
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
  lapply(sets, function(s) sort(setdiff(as.character(s), c(exposure, outcome))))
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
.print_model_comparison_list <- function(mods) {
  ##preferred path: modelsummary
  if (requireNamespace("modelsummary", quietly = TRUE)) {
    tab <- modelsummary::msummary(
      mods,
      stars = TRUE,#default stars
      output   = "markdown",#good for console
      gof_omit = "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma"
    )
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