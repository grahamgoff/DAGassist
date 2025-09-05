#' Run the DAGassist pipeline and produce a compact report (console/LaTeX/Word/Excel/Text)
#'
#' `DAGassist()` validates a DAG + model specification, classifies node roles,
#' builds minimal and canonical adjustment sets, fits comparable models, and
#' renders a compact report in several formats (console, LaTeX fragment, DOCX,
#' XLSX, plain text). It also supports passing a **single engine call** (e.g.
#' `feols(Y ~ X + Z | fe, data = df)`) instead of a plain formula.
#'
#'@param dag A **dagitty** object (see [dagitty::dagitty()]).
#'@param formula Either (a) a standard model formula `Y ~ X + ...`, or
#'  (b) a single **engine call** such as `feols(Y ~ X + Z | fe, data = df, ...)`.
#'  When an engine call is provided, `engine`, `data`, and extra arguments are
#'  automatically extracted from the call.
#'@param data A `data.frame` (or compatible--e.g. tibble); optional if supplied via the
#'  engine call in `formula`.
#'@param exposure Optional character scalar; if missing/empty, inferred from the
#'  DAG (must be unique).
#'@param outcome Optional character scalar; if missing/empty, inferred from the
#'  DAG (must be unique).
#'@param engine Modeling function, default [stats::lm]. Ignored if `formula`
#'  is a single engine call (in that case the function is taken from the call).
#'@param engine_args Named list of extra arguments forwarded to `engine(...)`.
#'  If `formula` is an engine call, arguments from the call are merged with
#'  `engine_args` (call values take precedence).
#'@param verbose Logical (default `TRUE`). Controls verbosity in the console
#'  printer (formulas + notes).
#'@param type Output type. One of
#'  `"console"` (default), `"latex"`/`"docx"`/`"word"`,
#'  `"excel"`/`"xlsx"`, `"text"`/`"txt"`.
#'@param out Output file path for the non-console types:
#'  * `type="latex"`: a **LaTeX fragment** written to `out` (must end with `.tex`).
#'  * `type="docx"`/`"word"`: a **Word (.docx)** file written to `out`.
#'  * `type="excel"`/`"xlsx"`: an **Excel (.xlsx)** file written to `out`.
#'  * `type="text"`/`"txt"`: a **plain-text** file written to `out`.
#'  Ignored for `type="console"`.
#'@param imply Logical; if `TRUE`, DAGassist highlights variables added by DAG
#'  logic (minimal/canonical sets) in the notes and marks canonical roles in
#'  the roles table. Models are *always* fit for minimal/canonical to allow
#'  side-by-side comparison, regardless of `imply`.
#'@param labels optional variable labels. can be named char vector or df or unnamed
#'  char vector
#'@details
#'    **Engine-call parsing.** If `formula` is a call (e.g., `feols(Y ~ X | fe, data=df)`),
#'  DAGassist extracts the engine function, the formula, the data argument, and
#'  any additional engine arguments directly from that call; these are merged with
#'  `engine`/`engine_args` you pass explicitly (call arguments win).
#'
#'    **Fixest tails.** For engines like **fixest** that use `|` to denote FE/IV
#'  parts, DAGassist preserves any `| ...` “tail” when constructing minimal or
#'  canonical formulas (e.g., `Y ~ X + controls | fe | iv(...)`).
#'
#'    **Output types.**
#'  * `console` prints* roles, sets, formulas (if `verbose`), and a compact model
#'   comparison with `{modelsummary}` if available, then falls back gracefully.
#'  * `latex` writes a **LaTeX fragment** you can `\input{}` into a paper.
#'  * `docx`/`word` writes a **Word** doc via Pandoc. It will use a reference
#'   document if set via `options(DAGassist.ref_docx="path/to/ref.docx")`, else
#'   falls back to a bundled or default reference DOCX, else Pandoc defaults.
#'  * `excel`/`xlsx` writes an **Excel** workbook with tidy tables.
#'  * `text`/`txt` writes a **plain-text** report suitable for logs/notes.
#'
#'    **Dependencies.** Minimal core requires `{dagitty}`. Optional enhancements:
#'  `{modelsummary}` (pretty tables), `{broom}` (fallback tidying), `{rmarkdown}`
#'  + **Pandoc** (DOCX), `{writexl}` (XLSX).
#'
#'@return An object of class `"DAGassist_report"`, invisibly for file outputs,
#'and printed for `type="console"`. The list contains:
#'  \itemize{
#'    \item `validation` — result from internal `validate_spec(...)` (includes `ok`).
#'    \item `roles` — roles data.frame from `classify_nodes(...)`.
#'    \item `bad_in_user` — variables in user controls that are mediator/collider/descendant of outcome.
#'    \item `controls_minimal` — (legacy) one minimal set (character vector).
#'    \item `controls_minimal_all` — list of all minimal sets (character vectors).
#'    \item `controls_canonical` — canonical set (character vector; may be empty).
#'    \item `formulas` — list with `original`, `minimal`, `minimal_list`, `canonical`.
#'    \item `models` — list with fitted models `original`, `minimal`, `minimal_list`, `canonical`.
#'    \item `verbose`, `imply` — flags as provided.
#'  }
#'
#'@section Errors and edge cases:
#'  * If exposure/outcome cannot be inferred uniquely, the function stops with a clear message.
#'  * Fitting errors (e.g., FE collinearity) are captured and displayed in comparisons
#'   without aborting the whole pipeline.
#'
#'@seealso [print.DAGassist_report()] for the console printer, and the helper
#'  exporters in `report_*` modules.
#'
#' @examplesIf requireNamespace("dagitty", quietly = TRUE)
#' \dontshow{set.seed(1)}
#' \dontshow{
#' # Build the DAG directly with dagitty
#' g <- dagitty::dagitty("dag { Z -> X; X -> M; X -> Y; M -> Y; Z -> Y; A -> Y; B -> Y; X -> C; Y -> C }")
#' dagitty::exposures(g) <- "X"; dagitty::outcomes(g) <- "Y"
#'
#' n <- 150
#' A <- rnorm(n); B <- rnorm(n); Z <- rnorm(n)
#' X <- 0.8*Z + rnorm(n)
#' M <- 0.9*X + rnorm(n)
#' Y <- 0.7*X + 0.6*M + 0.3*Z + 0.2*A - 0.1*B + rnorm(n)
#' C <- 0.5*X + 0.4*Y + rnorm(n)
#' df <- data.frame(A,B,Z,X,M,Y,C)
#' }
#' # generate a console DAGassist report
#' DAGassist(dag = g, formula = lm(Y ~ X + Z + C + M, data = df))
#'
#' # generate a LaTeX DAGassist report
#' \donttest{
#' DAGassist(dag = g, formula = lm(Y ~ X + Z + C + M, data = df),
#'           type = "latex", out = file.path(tempdir(), "frag.tex"))
#' }
#' @export

DAGassist <- function(dag, formula, data, exposure, outcome,
                      engine = stats::lm, engine_args = list(),
                      labels = NULL,
                      verbose = TRUE, 
                      type = c("console", "latex", "word", "docx", 
                                "excel", "xlsx", "text", "txt"), 
                      out = NULL,
                      imply = FALSE) {
  # set output type
  type <- match.arg(type)
  
  ## allow formula to be either a formula or a single engine call
  spec_expr <- substitute(formula)  # capture unevaluated argument
  parsed <- NULL
  
  if (is.call(spec_expr) && !identical(spec_expr[[1]], as.name("~"))) {
    # User passed an engine call like feols(y ~ x | fe, data = df, ...)
    parsed <- .extract_from_engine_call(spec_expr, eval_env = parent.frame())
    engine      <- parsed$engine
    formula     <- parsed$formula
    # only fill data if user didn't also pass `data=` explicitly
    if (missing(data) || is.null(data)) data <- parsed$data
    # merge engine_args: call args take precedence; user-supplied list can add/override
    engine_args <- utils::modifyList(parsed$engine_args, engine_args)
  } else {
    # User passed a plain formula; keep engine and data as provided
    # nothing to do here
  }
  
  ## infer exposure/outcome from DAG if user didn't set them
  xy <- .infer_xy(dag, exposure, outcome)
  exposure <- xy$exposure
  outcome  <- xy$outcome
  
  #validate inputs using the now-normalized pieces
  v <- validate_spec(dag, formula, data, exposure, outcome)
  if (!v$ok) return(list(validation = v))
  
  #classify nodes
  roles <- classify_nodes(dag, exposure, outcome)
  
  #normalize labels and prepare roles table
  labmap <- tryCatch(.normalize_labels(labels, vars = unique(roles$variable)),
                     error = function(e) { NULL })
  # drop \n from labs
  if (length(labmap)) {
    clean <- function(x) trimws(gsub("[\\r\\n]+", " ", x, perl = TRUE))
    labmap <- vapply(labmap, clean, character(1))
  }
  #make a display copy for exporters without touching internal names
  roles_display <- tryCatch(.apply_labels_to_roles_df(roles, labmap),
                            error = function(e) roles)
  
  # what controls did the user use? (only from the pre-| part if present)
  rhs_terms <- .rhs_terms_safe(formula)
  # only DAG nodes can be bad controls; ignore nuisance (eg fe, did, transforms)
  user_controls <- intersect(setdiff(rhs_terms, c(exposure)), roles$variable)
  
  # "bad controls"
  bad <- roles$variable[roles$is_mediator | roles$is_collider | roles$is_descendant_of_outcome]
  bad_in_user <- intersect(user_controls, bad)
  
  
  # all the minimal sets
  minimal_sets_all <- .minimal_sets_all(dag, exposure, outcome)
  # keep a single minimal for reference
  minimal <- if (length(minimal_sets_all)) minimal_sets_all[[1]] else character(0)
  
  # formulas for all minimal sets---preserve fixest tails
  f_mins <- lapply(minimal_sets_all, function(s) .build_formula_with_controls(formula, exposure, outcome, s))
  
  # canonical set and formula
  canonical <- .pick_canonical_controls(dag, exposure, outcome)
  f_canon   <- .build_formula_with_controls(formula, exposure, outcome, canonical)
  
  #fits: always show Original, every Minimal, and Canonical at the end 
  #if there are multiple min, push canonical to the end
  m_orig <- .safe_fit(engine, formula, data, engine_args)
  
  ## if auto add is true, imply the sets based on dag relationships,
  ## otherwise, keep lists empty and specify in the report list
  if (isTRUE(imply)) {
    if (is.data.frame(roles) && "variable" %in% names(roles)) {
      roles$canon <- ifelse(roles$variable %in% canonical, "x", "")
    }
  } else {
    if (is.data.frame(roles) && "canon" %in% names(roles)) roles$canon <- ""
  }
  
  # always fit alternates so display isn't tied to imply
  m_mins <- lapply(
    f_mins,
    function(fm) if (.same_formula(fm, formula)) m_orig else .safe_fit(engine, fm, data, engine_args)
  )
  
  m_canon <- if (.same_formula(f_canon, formula)) {
    m_orig
  } else {
    same_idx <- which(vapply(f_mins, function(fm) .same_formula(fm, f_canon), logical(1)))
    if (length(same_idx)) m_mins[[same_idx[1]]] else .safe_fit(engine, f_canon, data, engine_args)
  }

  ###list unevaluated nuisance vars
  .collect_rhs <- function(fml) .rhs_terms_safe(fml)
  
  rhs_all <- unique(unlist(c(
    list(.collect_rhs(formula)),
    lapply(f_mins, .collect_rhs),
    list(.collect_rhs(f_canon))
  )))
  #compare to DAG node names via roles$variable
  dag_nodes <- roles$variable
  unevaluated <- sort(setdiff(rhs_all, dag_nodes))
  #if exposure or outcome were somehow not in DAG unlist
  unevaluated <- setdiff(unevaluated, c(exposure, outcome))
  #pretty string for exporters
  unevaluated_str <- if (length(unevaluated)) paste(unevaluated, collapse = ", ") else ""
  
  
  report <- list(
    validation = v, 
    roles = roles,
    roles_display = roles_display,
    labels_map = labmap,
    bad_in_user = bad_in_user,
    controls_minimal = minimal, # keeps legacy single-min key
    controls_minimal_all = minimal_sets_all, # all minimal sets
    controls_canonical = canonical,
    formulas = list(
      original = formula,
      # so it prints multiple formulas if there are multiple minimal sets
      minimal = if (length(f_mins)) f_mins[[1]] else if (isTRUE(imply)) .build_minimal_formula(formula, exposure, outcome, minimal) else formula,
      minimal_list = f_mins, #  all minimal formulas
      canonical = f_canon
    ),
    models = list(
      original= m_orig,
      minimal = if (length(m_mins)) m_mins[[1]] else if (isTRUE(imply)) .safe_fit(engine, .build_minimal_formula(formula, exposure, outcome, minimal), data, engine_args) else m_orig,
      minimal_list = m_mins, # all minimal fits
      canonical = m_canon
    ),
    unevaluated = unevaluated,
    unevaluated_str = unevaluated_str,
    verbose = isTRUE(verbose),
    imply = isTRUE(imply)
  )
  class(report) <- c("DAGassist_report", class(report))
  # Build unified artifacts once for all outputs
  mods_full <- .build_named_mods(report)
  models_df_full <- .build_models_df(report)
  
  ##### LATEX OUT BRANCH #####
  if (type == "latex") {
    if (tolower(tools::file_ext(out)) == "docx") {
      stop("LaTeX fragment must not be written to a .docx path. Use a .tex filename.", call. = FALSE)
    }
    if (is.null(out)) stop("type='latex' requires `out=` file path.", call. = FALSE)
    
    res_min <- list(
      validation = list(
        status = if (isTRUE(v$ok)) "VALID" else "INVALID",
        issues = if (!is.null(v$issues)) v$issues else character(0)
      ),
      coef_rename = labmap,
      roles_df = report$roles_display,
      models_df = models_df_full,     
      models = mods_full,        
      min_sets = report$controls_minimal_all,
      canon = report$controls_canonical,
      unevaluated_str = report$unevaluated_str
    )
    .report_latex_fragment(res_min, out)
    return(invisible(structure(report, file = normalizePath(out, mustWork = FALSE))))
  }
  
  ##### WORD OUT BRANCH #####
  if (type %in% c("docx","word")) {
    res_min <- list(
      validation = list(
        status = if (isTRUE(v$ok)) "VALID" else "INVALID",
        issues = if (!is.null(v$issues)) v$issues else character(0)
      ),
      roles_df = report$roles_display,
      coef_rename = labmap,
      models = mods_full,                
      min_sets = report$controls_minimal_all,
      canon = report$controls_canonical,
      unevaluated_str = report$unevaluated_str
    )
    return(.report_docx(res_min, out))
  }
  
  #### EXCEL OUT BRANCH ####
  if (type %in% c("excel","xlsx")) {
    res_min <- list(
      roles_df = report$roles_display,
      coef_rename = labmap,
      models = mods_full,         
      min_sets = report$controls_minimal_all,
      canon = report$controls_canonical,
      unevaluated_str = report$unevaluated_str
    )
    .report_xlsx(res_min, out)
    return(invisible(structure(report, file = normalizePath(out, mustWork = FALSE))))
  }
  
  ##### TEXT OUT BRANCH #####
  if (type %in% c("text","txt")) {
    res_min <- list(
      roles_df = report$roles_display,
      coef_rename = labmap,
      models = mods_full,         
      min_sets = report$controls_minimal_all,
      canon = report$controls_canonical,
      unevaluated_str = report$unevaluated_str
    )
    .report_txt(res_min, out)
    return(invisible(structure(report, file = normalizePath(out, mustWork = FALSE))))
  }
  
  report
}

#' Print method for DAGassist reports
#'
#' Nicely prints the roles table, highlights potential bad controls, shows
#' minimal/canonical adjustment sets, optionally shows formulas, and renders a
#' compact model comparison (using `{modelsummary}` if available, falling back
#' to `{broom}` or basic `coef()` preview).
#'
#' @param x A `"DAGassist_report"` object returned by [DAGassist()].
#' @param ... Additional arguments (currently unused; present for S3 compatibility).
#'
#' @details
#' The printer respects the `verbose` flag in the report: when `TRUE`, it
#' includes formulas and a brief note on variables added by DAG logic (minimal
#' and canonical sets). Fitting errors are shown inline per model column and do
#' not abort printing.
#'
#' @return Invisibly returns `x`.
#'
#' @export
 
print.DAGassist_report <- function(x, ...) {
  
  cat(clr_bold(clr_blue("DAGassist Report:")), "\n")
  
  if (!x$validation$ok) { print(x$validation); return(invisible(x)) }
  
  #set verbose flag for suppressing certain parts
  verbose <- if (is.null(x$verbose)) TRUE else isTRUE(x$verbose)
  
  cat("\nRoles:\n")
  # Only label the variable names; keep the logical flags intact for the S3 printer
  r <- tryCatch(
    .apply_labels_to_roles_df(x$roles, x$labels_map),
    error = function(e) x$roles
  )
  
  # ensure the class is still there (usually preserved, but this is harmless)
  if (!inherits(r, "DAGassist_roles")) {
    class(r) <- unique(c("DAGassist_roles", class(r)))
  }
  
  print(r)  # <- do NOT call .roles_pretty() here
  
  if (length(x$bad_in_user)) {
    cat(clr_red("\n (!) Bad controls in your formula: {", paste(x$bad_in_user, collapse = ", "), "}\n", sep = ""))
  } else {
    cat(clr_green("\nNo bad controls detected in your formula.\n"))
  }
  
  # compare adjustment sets
  if (length(x$controls_minimal_all)) {
    for (i in seq_along(x$controls_minimal_all)) {
      cat("Minimal controls ", i, ": ", .format_set(x$controls_minimal_all[[i]]), "\n", sep = "")
    }
  } else {
    cat("Minimal controls 1: {}\n")
  }
  cat("Canonical controls: ", .format_set(x$controls_canonical), "\n", sep = "")
  
  if (length(x$unevaluated)) {
    cat("\nNote: The following regressors, which are included in the below ",
        "models, were not evaluated by DAGassist because they are not nodes in the DAG:\n  {",
        x$unevaluated_str, "}\n", sep = "")
  }
  
  if(verbose){
  # compare formulas
    cat(clr_bold("\nFormulas:\n", sep = ""))
    cat("  original:  ",  deparse(x$formulas$original),  "\n", sep = "")
    if (isTRUE(x$imply)) {
      
      if (length(x$formulas$minimal_list)) {
        for (i in seq_along(x$formulas$minimal_list)) {
          cat("  minimal ", sprintf("%-2d", i), ": ", deparse(x$formulas$minimal_list[[i]]), "\n", sep = "")
       }
      } else {
      cat("  minimal 1: ", deparse(x$formulas$minimal), "\n", sep = "")
      }
      cat("  canonical: ",  deparse(x$formulas$canonical), "\n", sep = "")
  
      ## note if specs are identical --check all pairs and sets
      mins_fmls <- if (length(x$formulas$minimal_list)) x$formulas$minimal_list else list(x$formulas$minimal)
      #initialize empty value for later
      pairs <- character(0)
  
      # original vs each minimal 
      for (i in seq_along(mins_fmls)) {
        if (.same_formula(x$formulas$original, mins_fmls[[i]])) {
          pairs <- c(pairs, sprintf("Original = Minimal %d", i))
        }
      }
  
      # canonical vs original
      if (.same_formula(x$formulas$canonical, x$formulas$original)) {
        pairs <- c(pairs, "Canonical = Original")
      }
  
    # canonical vs each minimal 
      for (i in seq_along(mins_fmls)) {
        if (.same_formula(x$formulas$canonical, mins_fmls[[i]])) {
          pairs <- c(pairs, sprintf("Canonical = Minimal %d", i))
        }
      }
  
    # minimal vs minimal 
      if (length(mins_fmls) > 1) {
        for (i in seq_len(length(mins_fmls) - 1L)) {
          for (j in seq.int(i + 1L, length(mins_fmls))) {
            if (.same_formula(mins_fmls[[i]], mins_fmls[[j]])) {
              pairs <- c(pairs, sprintf("Minimal %d = Minimal %d", i, j))
            }
          }
        }
      }
  
      if (length(pairs)) {
        cat(clr_yellow("\nNote: some specifications are identical (",
            paste(pairs, collapse = "; "),
            ").\nEstimates will match for those columns.\n", sep = ""))
      }
  
      ## Concise note about DAG-derived additions 
  
      # user RHS terms from the original pre-| formula
      user_rhs <- .rhs_terms_safe(x$formulas$original)
  
      exp_nm <- get_by_role(x$roles, "exposure")
      out_nm <- get_by_role(x$roles, "outcome")
  
      # build one short line per column that added variables
      lines <- character(0)
      drop_exp <- if (!is.na(exp_nm) && nzchar(exp_nm)) exp_nm else character(0)
  
      mins_fmls <- if (length(x$formulas$minimal_list)) x$formulas$minimal_list else list(x$formulas$minimal)
      for (i in seq_along(mins_fmls)) {
        rhs_i  <- setdiff(.rhs_terms_safe(mins_fmls[[i]]), drop_exp)
        added  <- setdiff(rhs_i, user_rhs)
        if (length(added)) lines <- c(lines, sprintf("  - Minimal %d added: %s", i, .format_set(added)))
       }
  
      rhs_c    <- setdiff(.rhs_terms_safe(x$formulas$canonical), drop_exp)
      added_c  <- setdiff(rhs_c, user_rhs)
      if (length(added_c)) lines <- c(lines, sprintf("  - Canonical added: %s", .format_set(added_c)))
  
      if (length(lines)) {
        if (!is.na(exp_nm) && nzchar(exp_nm) && !is.na(out_nm) && nzchar(out_nm)) {
          cat(clr_bold(clr_yellow("\nNote: DAGassist added variables not in your formula, based on the\nrelationships in your DAG, ",
              "to block back-door paths\nbetween ", exp_nm, " and ", out_nm, ".\n", sep = "")))
        } else {
          cat(clr_bold(clr_yellow("\nNote: DAGassist added variables not in your formula, based on the\nrelationships in your DAG, ",
              "to block back-door paths.\n", sep = "")))
        }
        cat(paste(lines, collapse = "\n"), "\n", sep = "")
      }
    }
  } # end of auto add
  
  ## build the model list for modelsummary/broom
  mods <- list("Original" = x$models$original)
  if (length(x$models$minimal_list)) {
    for (i in seq_along(x$models$minimal_list)) {
      mods[[sprintf("Minimal %d", i)]] <- x$models$minimal_list[[i]]
    }
  } else {
    mods[["Minimal 1"]] <- x$models$minimal
  }
  mods[["Canonical"]] <- x$models$canonical
  
  .print_model_comparison_list(mods, coef_rename = x$labels_map)
}