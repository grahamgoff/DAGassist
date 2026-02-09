#' Generate and/or export report classifying nodes and comparing models
#'
#' `DAGassist()` validates a DAG + model specification, classifies node roles,
#' builds minimal and canonical adjustment sets, fits comparable models, and
#' renders a compact report in several formats (console, LaTeX fragment, DOCX,
#' XLSX, plain text). It also supports recovering new estimands, such as the
#' sample average treatment effect (SATE) or the sample average controlled direct 
#' effect (SACDE).
#' 
#' @param dag A **dagitty** object (see [dagitty::dagitty()]).
#' @param formula Either (a) a standard model formula `Y ~ X + ...`, or
#'   (b) a single **engine call** such as `feols(Y ~ X + Z | fe, data = df, ...)`.
#'   When an engine call is provided, `engine`, `data`, and extra arguments are
#'   automatically extracted from the call.
#' @param data A `data.frame` (or compatible, e.g. tibble). Optional if supplied
#'   via the engine call in `formula`.
#' @param exposure Optional character scalar; if missing/empty, inferred from the
#'   DAG (must be unique).
#' @param outcome Optional character scalar; if missing/empty, inferred from the
#'   DAG (must be unique).
#' @param engine Modeling function, default [stats::lm]. Ignored if `formula`
#'   is a single engine call (in that case the function is taken from the call).
#' @param engine_args Named list of extra arguments forwarded to `engine(...)`.
#'   If `formula` is an engine call, arguments from the call are merged with
#'   `engine_args` (call values take precedence).
#' @param verbose Logical (default `TRUE`). Controls verbosity in the console
#'   printer (formulas + notes).
#' @param type Output type. One of
#'   `"console"` (default), `"latex"`/`"docx"`/`"word"`,
#'   `"excel"`/`"xlsx"`, `"text"`/`"txt"`,
#'   or the plotting types `"dwplot"`/`"dotwhisker"`.
#'   For `type = "latex"`, if no `out=` is supplied, a LaTeX fragment is printed
#'   to the console instead of being written to disk.
#' @param out Output file path for the non-console types:
#'   * `type="latex"`: a **LaTeX fragment** written to `out` (usually `.tex`);
#'     when omitted, the fragment is printed to the console.
#'   * `type="text"`/`"txt"`: a **plain-text** file written to `out`;
#'     when omitted, the report is printed to console.
#'   * `type="dotwhisker"`/`"dwplot"`: a **image (.png)** file written to `out`;
#'     when omitted, the plot is rendered within RStudio.
#'   * `type="docx"`/`"word"`: a **Word (.docx)** file written to `out`.
#'   * `type="excel"`/`"xlsx"`: an **Excel (.xlsx)** file written to `out`.
#'   Ignored for `type="console"`.
#' @param imply Logical; default `FALSE`. Specifies **evaluation scope.**
#'   - If `FALSE` (default): restrict DAG evaluation to variables **named in the formula**
#'     (prune the DAG to exposure, outcome, and RHS terms). Roles/sets/bad-controls are
#'     computed on this pruned graph, and the roles table **only** shows those variables.
#'     Essentially, it fits the DAG to the formula. 
#'   - If `TRUE`: evaluate on the **full DAG** and allow DAG-implied controls in the
#'     minimal/canonical sets. The roles table shows all DAG nodes, and the printout 
#'     notes any variables added beyond your RHS. Essentially, it fits the formula to the DAG.
#' @param labels Optional variable labels (named character vector or data.frame).
#' @param omit_intercept Logical; drop intercept rows from the model comparison display (default `TRUE`).
#' @param omit_factors Logical; drop factor-level rows from the model comparison display (default `TRUE`).
#'    This parameter only suppresses factor **output**--they are still included in the regression. 
#' @param show Which sections to include in the output. One of `"all"` (default),
#'    `"roles"` (only the roles grid), or `"models"` (only the model comparison table/plot).
#'    This makes it possible to generate and export just roles or just comparisons.
#' @param eval_all Logical; default `FALSE`.  When `TRUE`, keep **all original RHS terms** 
#'    that are not in the DAG (e.g., fixed effects, interactions, splines, 
#'    convenience covariates) in the minimal and canonical formulas. 
#'    When `FALSE` (default), RHS terms not present as DAG nodes are dropped 
#'    from those derived formulas.
#' @param bivariate Logical; if `TRUE`, include a bivariate (exposure-only) specification
#'    in the comparison table **in addition** to the user's original and DAG-derived models.
#' @param exclude Optional character vector to remove neutral controls from the canonical set.
#'    Recognized values are `"nct"` (drop *neutral-on-treatment* controls) and
#'    `"nco"` (drop *neutral-on-outcome* controls). You can supply one or both,
#'    e.g. `exclude = c("nco", "nct")`; each requested variant is fitted and shown
#'    as a separate "Canon. (-...)" column in the console/model exports.
#' @param estimand Character; causal estimand. Currently only
#'    supported for `type = "console"`. One of `"raw"` (default), `"SATE"`, `"SATT"`, or `"SACDE"`;
#'    uses the \pkg{WeightIt} package to add weighted versions of each comparison
#'    model as additional columns. For models with mediators, `"SACDE"` links 
#'    to the \pkg{DirectEffects} to for a controlled direct effect via sequential g-estimation.
#' @param weights_args List; parameters for weighting package. `DAGassist` is agnostic
#'    and passes list directly to the respective weighting package 
#' @param auto_acde Logical; if `TRUE` (default), automates handling conflicts between specifications
#'    and estimand arguments. Fails gracefully with a helpful error when users specify ACDE estimand
#'    for a model without mediators.
#' @param acde List; options for the controlled direct effect workflow (estimands `"SACDE"`/`"SCDE"`).
#'   Users can override parts of the sequential g-estimation specification with named elements:
#'   `m` (mediators), `x` (baseline covariates), `z` (intermediate covariates),
#'   `fe` (fixed-effects variables), `fe_as_factor` (wrap `fe` as `factor()`), and
#'   `include_descendants` (treat descendants of mediators as mediators). 
#' @param directeffects_args Named list of arguments forwarded to [DirectEffects::sequential_g()]
#'   when `estimand` includes `"ACDE"`/`"CDE"` (e.g., simulation/bootstrap controls,
#'   variance estimator options).
#' @details
#' **Engine-call parsing.** If `formula` is a call (e.g., `feols(Y ~ X | fe, data=df)`),
#' DAGassist extracts the engine function, formula, data argument, and any additional
#' engine arguments directly from that call; these are merged with `engine`/`engine_args`
#' you pass explicitly (call arguments win).
#'
#' **fixest tails.** For engines like **fixest** that use `|` to denote FE/IV parts,
#' DAGassist preserves any `| ...` tail when constructing minimal/canonical formulas
#' (e.g., `Y ~ X + controls | fe | iv(...)`).
#'
#' **Roles grid.** The roles table displays short headers:
#'   - `Exp.` (exposure), 
#'   - `Out.` (outcome),
#'   - `CON` (confounder),
#'   - `MED` (mediator),
#'   - `COL` (collider),
#'   - `dOut` (descendant of `Y`),
#'   - `dMed` (descendant of any mediator),
#'   - `dCol` (descendant of any collider),
#'   - `dConfOn` (descendant of a confounder **on** a back-door path),
#'   - `dConfOff` (descendant of a confounder **off** a back-door path),
#'   - `NCT` (neutral control on treatment),
#'   - `NCO` (neutral control on outcome).
#'   These extra flags are used to (i) warn about bad controls, and (ii) build
#'   filtered canonical sets such as “Canonical (–NCO)” for export.
#'
#' **Bad controls.** For total-effect estimation, DAGassist flags as `bad controls`
#' any variables that are `MED`, `COL`, `dOut`, `dMed`, or `dCol`. These are warned in
#' the console and omitted from the model-comparison table. Valid confounders (pre-treatment)
#' are eligible for minimal/canonical adjustment sets.
#'
#' **Output types.**
#' * `console` prints roles, adjustment sets, formulas (if `verbose`), and a compact model comparison
#'   (using `{modelsummary}` if available, falling back gracefully otherwise).
#' * `latex` writes or prints a **LaTeX fragment** you can `\\input{}` into a paper —
#'   it uses `tabularray` long tables and will include any requested Canon. (-NCO / -NCT) variants.
#' * `docx`/`word` writes a **Word** doc (respects `options(DAGassist.ref_docx=...)` if set).
#' * `excel`/`xlsx` writes an **Excel** workbook with tidy tables.
#' * `text`/`txt` writes a **plain-text** report for logs/notes.
#' * `dwplot`/`dotwhisker` produces a dot-whisker visualization of the fitted models.
#'
#' **Dependencies.** Core requires `{dagitty}`. Optional enhancements: `{modelsummary}`
#' (pretty tables), `{broom}` (fallback tidying), `{rmarkdown}` + **pandoc** (DOCX),
#' `{writexl}` (XLSX), `{dotwhisker}`/`{ggplot2}` for plotting.
#'
#' @return An object of class `"DAGassist_report"`, invisibly for file and plot
#' outputs, and printed for `type="console"`. The list contains:
#' \itemize{
#'   \item `validation` - result from `validate_spec(...)` which verifies acyclicity and X/Y declarations.
#'   \item `roles` - raw roles data.frame from `classify_nodes(...)` (logic columns).
#'   \item `roles_display` - roles grid after labeling/renaming for exporters.
#'   \item `bad_in_user` - variables in the user's RHS that are `MED`/`COL`/`dOut`/`dMed`/`dCol`.
#'   \item `controls_minimal` - (legacy) one minimal set (character vector).
#'   \item `controls_minimal_all` - list of all minimal sets (character vectors).
#'   \item `controls_canonical` - canonical set (character vector; may be empty).
#'   \item `controls_canonical_excl` - named list of filtered canonical sets
#'     (e.g. `$nco`, `$nct`) when `exclude` is used.
#'   \item `formulas` - list with `original`, `minimal`, `minimal_list`, `canonical`,
#'     and any filtered canonical formulas.
#'   \item `models` - list with fitted models `original`, `minimal`, `minimal_list`,
#'     `canonical`, and any filtered canonical models.
#'   \item `verbose`, `imply` - flags as provided.
#' }
#'
#' @section Interpreting the output:
#' See the vignette articles for worked examples on generating roles-only, models-only,
#' and LaTeX/Word/Excel reports.
#' 
#' **Model Comparison:**
#' \itemize{
#'   \item **Minimal** - the smallest adjustment set that blocks all back-door paths
#'         (confounders only).
#'   \item **Canonical** - the largest permissible set: includes all controls that are not
#'         `MED`, `COL`, `dOut`, `dMed`, or `dCol`. 
#' }
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
#' DAGassist(dag = g, 
#'           formula = lm(Y ~ X + Z + C + M, data = df))
#'
#' # generate a LaTeX DAGassist report in console
#' DAGassist(dag = g, 
#'           formula = lm(Y ~ X + Z + C + M, data = df),
#'           type = "latex")
#' 
#' # generate just the roles table in the console
#' DAGassist(dag = g, 
#'           show = "roles")
#' @export

DAGassist <- function(dag, 
                      formula = NULL, 
                      data = NULL, 
                      exposure, 
                      outcome,
                      engine = stats::lm, 
                      labels = NULL,
                      verbose = TRUE, 
                      type = c("console", "latex", "word", "docx", 
                                "excel", "xlsx", "text", "txt", 
                               "dwplot", "dotwhisker"), 
                      show = c("all", "roles", "models"),
                      out = NULL,
                      imply = FALSE,
                      eval_all = FALSE,
                      exclude = NULL,
                      omit_intercept = TRUE,
                      omit_factors = TRUE,
                      bivariate = FALSE,
                      estimand = c("raw", "none", "SATE", "SATT", "SACDE", "SCDE"),
                      engine_args = list(),
                      weights_args = list(),
                      auto_acde = TRUE,
                      acde = list(),
                      directeffects_args = list()) {
  # set output type
  type <- match.arg(type)
  # set show type
  show <- match.arg(show)
  
  # fail quickly if show = models and no formula
  if (show == "models" && (missing(formula) || is.null(formula))) {
    stop("show='models' requires a model specification (formula or engine call).", call. = FALSE)
  }
  
  #ensure default to raw when no estimand arg is passed
  #and llow multiple estimands (e.g., c("ATE","ACDE"))
  .allowed_estimands <- c("raw", "none", "SATE", "SATT", "SACDE", "SCDE")
  # if estimand=NULL, default to raw. do not default to multi-estimand
  if (missing(estimand) || is.null(estimand) || length(estimand) == 0L) {
    estimand <- "raw"
  } else {
    estimand <- match.arg(estimand, choices = .allowed_estimands, several.ok = TRUE)
  }
  
  estimand_requested <- estimand
  estimand <- .dagassist_normalize_estimand(estimand)
  
  acde <- .dagassist_normalize_acde_spec(acde)
  
  
  ###### FAST-PATH FOR ROLES ONLY OUTPUT TO NOT REQUIRE FORMULA OR DATA ########
  roles_only_no_formula <- identical(show, "roles") && (missing(formula) || is.null(formula))
  if (roles_only_no_formula) {
    # infer exposure/outcome if not provided
    xy <- .infer_xy(dag, exposure, outcome)
    exposure <- xy$exposure
    outcome  <- xy$outcome
    # compute roles directly from DAG 
    roles <- classify_nodes(dag, exposure, outcome)
    #detect m bias/butterfly bias
    conditions <- tryCatch(.detect_dag_conditions(roles), error = function(e) list())
    #label normalization
    labmap <- tryCatch(.normalize_labels(labels, vars = unique(roles$variable)),
                       error = function(e) NULL)
    if (length(labmap)) {
      clean <- function(x) trimws(gsub("[\\r\\n]+", " ", x, perl = TRUE))
      labmap <- vapply(labmap, clean, character(1))
    }
    roles_display <- tryCatch(.apply_labels_to_roles_df(roles, labmap),
                              error = function(e) roles)
    
    # build a report object with same shape
    v <- list(ok = TRUE, issues = new_issue_table())
    report <- list(
      validation = v,
      roles = roles,
      roles_display = roles_display,
      labels_map = labmap,
      bad_in_user = character(0),
      controls_minimal = character(0),
      controls_minimal_all = list(),
      controls_canonical = character(0),
      controls_canonical_excl = character(0),
      conditions = conditions,  
      formulas = list(
        original = NULL,
        minimal = NULL,
        minimal_list = list(),
        canonical = NULL,
        canonical_excl = NULL
      ),
      models = list(
        original = NULL,
        minimal = NULL,
        minimal_list = list(),
        canonical = NULL,
        canonical_excl = NULL
      ),
      unevaluated = character(0),
      unevaluated_str = "",
      verbose = isTRUE(verbose),
      imply   = isTRUE(imply)
    )
    report$settings <- list(
      omit_intercept = isTRUE(omit_intercept),
      omit_factors = isTRUE(omit_factors),
      show = show
    )
    report$.__data <- if (!is.null(data)) data else NULL
    report$settings$coef_omit <- .build_coef_omit(
      data = report$.__data,
      omit_intercept = report$settings$omit_intercept,
      omit_factors   = report$settings$omit_factors
    )
    class(report) <- c("DAGassist_report", class(report))
    
    # build objects for exporters
    mods_full <- .build_named_mods(report)
    models_df_full <- .build_models_df(report)
    
    # export to file or return to console
    file_attr <- if (!is.null(out)) normalizePath(out, mustWork = FALSE) else NULL
    
    if (type == "latex") {
      res_min <- list(
        validation = list(status = "VALID", issues = character(0)),
        coef_rename = labmap,
        coef_omit = report$settings$coef_omit,
        roles_df = report$roles_display,
        models_df = models_df_full,
        models = mods_full,
        min_sets = report$controls_minimal_all,
        canon = report$controls_canonical,
        unevaluated_str= report$unevaluated_str,
        show = show,
        verbose = verbose
      )
      
      if (!is.null(out)) {
        # safety check when path is specified
        if (tolower(tools::file_ext(out)) == "docx")
          stop("LaTeX fragment must not be written to a .docx path. Use a .tex filename.", call. = FALSE)
        
        .report_latex_fragment(res_min, out)
        return(invisible(structure(report, file = normalizePath(out, mustWork = FALSE))))
      } else {
        # no path -> print to console
        .report_latex_fragment(res_min, out = NULL)
        return(invisible(report))
      }
    }
    
    if (type %in% c("docx","word")) {
      res_min <- list(
        roles_df= report$roles_display,
        coef_omit = report$settings$coef_omit,
        coef_rename = labmap,
        models = mods_full,
        min_sets = report$controls_minimal_all,
        canon = report$controls_canonical,
        unevaluated_str= report$unevaluated_str,
        show = show,
        verbose = verbose
      )
      .report_docx(res_min, out)
      return(invisible(structure(report, file = file_attr)))
    }
    
    if (type %in% c("excel","xlsx")) {
      res_min <- list(
        roles_df = report$roles_display,
        coef_omit = report$settings$coef_omit,
        coef_rename = labmap,
        models = mods_full,
        min_sets = report$controls_minimal_all,
        canon = report$controls_canonical,
        unevaluated_str= report$unevaluated_str,
        show = show,
        verbose=verbose
      )
      .report_xlsx(res_min, out)
      return(invisible(structure(report, file = file_attr)))
    }
    
    if (type %in% c("text","txt")) {
      res_min <- list(
        roles_df= report$roles_display,
        coef_omit = report$settings$coef_omit,
        coef_rename = labmap,
        models = mods_full,
        min_sets = report$controls_minimal_all,
        canon = report$controls_canonical,
        unevaluated_str= report$unevaluated_str,
        show = show,
        verbose=verbose
      )
      .report_txt(res_min, out)
      return(invisible(structure(report, file = file_attr)))
    }
    
    # return report for print function
    return(report)
  }

  ### resume normal processing
  
  ## allow formula to be either a formula or a single engine call
  spec_expr <- substitute(formula)  # capture unevaluated argument
  parsed <- NULL
  
  if (is.call(spec_expr) && !identical(spec_expr[[1]], as.name("~"))) {
    # User passed an engine call like feols(y ~ x | fe, data = df, ...)
    parsed <- .extract_from_engine_call(spec_expr, eval_env = parent.frame())
    engine <- parsed$engine
    formula <- parsed$formula
    # only fill data if user didn't also pass `data=` explicitly
    if (missing(data) || is.null(data)) data <- parsed$data
    # merge engine_args: call args take precedence; user-supplied list can add/override
    ##this keeps it from crashing with modelsummary error if either side is not a list
    engine_args <- utils::modifyList(
      if (is.list(parsed$engine_args)) parsed$engine_args else list(),
      if (is.list(engine_args)) engine_args else list())
  } else {
    # User passed a plain formula; keep engine and data as provided
    # nothing to do here
  }
  #if the engine is fixest and call did not specify notes param, suppress notes 
  if (.dagassist_engine_is_fixest(engine) && is.null(engine_args$notes)) {
    engine_args$notes <- FALSE
  }
  ## infer exposure/outcome from DAG if user didn't set them
  xy <- .infer_xy(dag, exposure, outcome)
  exposure <- xy$exposure
  outcome <- xy$outcome
  
  #validate inputs using the now-normalized pieces
  v <- validate_spec(dag, formula, data, exposure, outcome)
  if (!v$ok) return(list(validation = v))
  
  #identify vars in user formula
  rhs_terms0 <- .rhs_terms_safe(formula)
  vars_in_formula <- unique(c(exposure, outcome, intersect(rhs_terms0, names(dag))))
  
  # pick which DAG to evaluate on depending on imply param
  dag_eval <- if (!isTRUE(imply)) .restrict_dag_to(dag, vars_in_formula) else dag
  
  # classify nodes on the evaluation DAG (PRUNED when imply = FALSE)
  roles <- classify_nodes(dag_eval, exposure, outcome)
  
  # auto-map ATE -> ACDE if the user specification conditions on mediator(s)
  estimand <- .dagassist_apply_auto_acde(
    estimand = estimand,
    formula = formula,
    roles = roles,
    auto_acde = auto_acde,
    include_descendants = isTRUE(acde$include_descendants)
  )
  
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
  # extras from user RHS that are NOT in the DAG (unevaluated by roles)
  rhs_extras <- setdiff(rhs_terms, roles$variable)
  # only DAG nodes can be bad controls; ignore nuisance (eg fe, did, transforms)
  user_controls <- intersect(setdiff(rhs_terms, c(exposure)), roles$variable)
  
  # "bad controls"
  bad <- roles$variable[
    roles$is_mediator |
      roles$is_collider |
      roles$is_descendant_of_outcome |
      roles$is_descendant_of_mediator |   
      roles$is_descendant_of_collider 
  ]
  bad_in_user <- intersect(user_controls, bad)
  # all the minimal sets
  minimal_sets_all <- .minimal_sets_all(dag_eval, exposure, outcome)
  # canonical set and formula
  canonical <- .pick_canonical_controls(dag_eval, exposure, outcome)
  # possibly several exclusion variants, e.g. c("nco","nct")
  canon_excl_list <- list()
  if (!is.null(exclude)) {
    exc <- unique(as.character(exclude))
    valid <- exc[exc %in% c("nct", "nco")]
    if (length(valid)) {
      for (excl in valid) {
        if (identical(excl, "nct")) {
          drop_vars <- roles$variable[roles$is_neutral_on_treatment]
        } else { # "nco"
          drop_vars <- roles$variable[roles$is_neutral_on_outcome]
        }
        canon_excl_list[[excl]] <- setdiff(canonical, drop_vars)
      }
    } else {
      warning("`exclude` must be NULL, 'nct', or 'nco'. Ignoring.", call. = FALSE)
    }
  }
  ## respect the formula (only use variables in the formula) when imply=FALSE
  # identify variables actually in user formula
  vars_in_formula <- unique(c(exposure, outcome, user_controls))
  ## sets for display. intersect with user RHS when imply = FALSE
  #minimal
  sets_min_for_use <- if (isTRUE(imply)) {
    minimal_sets_all
  } else {
    lapply(minimal_sets_all, function(s) intersect(s, user_controls))
  }
  #canonical
  canon_for_use <- if (isTRUE(imply)) {
    canonical
  } else {
    intersect(canonical, user_controls)
  }
  #for the exclude param
  canon_excl_for_use <- list()
  if (length(canon_excl_list)) {
    for (nm in names(canon_excl_list)) {
      canon_excl_for_use[[nm]] <- if (isTRUE(imply)) {
        canon_excl_list[[nm]]
      } else {
        intersect(canon_excl_list[[nm]], user_controls)
      }
    }
  }
  # limit roles table to vars in the formula 
  #with labels
  roles_display_formula <- tryCatch(
    .apply_labels_to_roles_df(
      roles[roles$variable %in% vars_in_formula, , drop = FALSE],
      labmap
    ),
    error = function(e) roles[roles$variable %in% vars_in_formula, , drop = FALSE]
  )
  # choose which roles table to show
  if (!isTRUE(imply)) {
    roles_display <- roles_display_formula
  }
  
  #build formulas
  minimal <- if (length(sets_min_for_use)) sets_min_for_use[[1]] else character(0)
  
  # formulas for all minimal sets---preserve fixest tails
  f_mins <- lapply(
    sets_min_for_use,
    function(s) {
      s2 <- if (isTRUE(eval_all)) unique(c(s, rhs_extras)) else s
      .build_formula_with_controls(formula, exposure, outcome, s2)
    }
  )
  #changed to accomodate eval_all
  f_canon <- {
    c2 <- if (isTRUE(eval_all)) unique(c(canon_for_use, rhs_extras)) else canon_for_use
    .build_formula_with_controls(formula, exposure, outcome, c2)
  }
  #canonical with some (on treatment/outcome) neutral controls removed
  f_canon_excl <- list()
  if (length(canon_excl_for_use)) {
    for (nm in names(canon_excl_for_use)) {
      c3 <- canon_excl_for_use[[nm]]
      c3 <- if (isTRUE(eval_all)) unique(c(c3, rhs_extras)) else c3
      f_canon_excl[[nm]] <- .build_formula_with_controls(formula, exposure, outcome, c3)
    }
  }
  if (!length(f_canon_excl)) f_canon_excl <- NULL
  #fits: always show Original, every Minimal, and Canonical at the end 
  #if there are multiple min, push canonical to the end
  m_orig <- .safe_fit(engine, formula, data, engine_args)
  # optional bivariate: Y ~ X (+ any fixest | tail), no DAG controls
  f_bivar <- if (isTRUE(bivariate)) {
    .build_formula_with_controls(formula, exposure, outcome, controls = character(0))
  } else NULL
  # fit the bivariate model if requested and not identical to Original
  m_bivar <- if (isTRUE(bivariate) && !.same_formula(f_bivar, formula)) {
    .safe_fit(engine, f_bivar, data, engine_args)
  } else {
    NULL
  }
  ## if auto add is true, imply the sets based on dag relationships,
  ## otherwise, keep lists empty and specify in the report list
  # modified to handle exclude params
  if (isTRUE(imply)) {
    if (is.data.frame(roles) && "variable" %in% names(roles)) {
      roles$canon <- ifelse(roles$variable %in% canonical, "x", "")
      if (length(canon_excl_list)) {
        if ("nct" %in% names(canon_excl_list)) {
          roles$canon_no_nct <- ifelse(roles$variable %in% canon_excl_list[["nct"]], "x", "")
        }
        if ("nco" %in% names(canon_excl_list)) {
          roles$canon_no_nco <- ifelse(roles$variable %in% canon_excl_list[["nco"]], "x", "")
        }
      }
    }
  } else {
    if (is.data.frame(roles) && "canon" %in% names(roles)) roles$canon <- ""
    if (is.data.frame(roles) && "canon_no_nct" %in% names(roles)) roles$canon_no_nct <- ""
    if (is.data.frame(roles) && "canon_no_nco" %in% names(roles)) roles$canon_no_nco <- ""
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
  #fit models for exclude neutral param
  m_canon_excl <- NULL
  if (length(f_canon_excl)) {
    m_canon_excl <- list()
    for (nm in names(f_canon_excl)) {
      fce <- f_canon_excl[[nm]]
      fit_nm <- NULL
      if (.same_formula(fce, formula)) {
        fit_nm <- m_orig
      } else {
        # reuse a minimal fit if identical
        same_idx2 <- which(vapply(f_mins, function(fm) .same_formula(fm, fce), logical(1)))
        if (length(same_idx2)) {
          fit_nm <- m_mins[[same_idx2[1]]]
        } else {
          fit_nm <- .safe_fit(engine, fce, data, engine_args)
        }
      }
      m_canon_excl[[nm]] <- fit_nm
    }
  }
  ###list unevaluated nuisance vars
  .collect_rhs <- function(fml) .rhs_terms_safe(fml)
  rhs_all <- unique(unlist(c(
    list(.collect_rhs(formula)),
    lapply(f_mins, .collect_rhs),
    list(.collect_rhs(f_canon)),
    if (isTRUE(bivariate) && !.same_formula(f_bivar, formula)) list(.collect_rhs(f_bivar)) else NULL
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
    dag = dag_eval,
    labels_map = labmap,
    bad_in_user = bad_in_user,
    
    controls_minimal = minimal, # keeps legacy single-min key
    controls_minimal_all = sets_min_for_use, # all minimal sets
    controls_canonical = canon_for_use,
    controls_canonical_excl = canon_excl_for_use,
    
    formulas = list(
      original = formula,
      bivariate  = if (isTRUE(bivariate) && !.same_formula(f_bivar, formula)) f_bivar else NULL,
      # so it prints multiple formulas if there are multiple minimal sets
      minimal = if (length(f_mins)) f_mins[[1]] else if (isTRUE(imply)) .build_minimal_formula(formula, exposure, outcome, minimal) else formula,
      minimal_list = f_mins, #  all minimal formulas
      canonical = f_canon,
      canonical_excl = f_canon_excl 
    ),
    models = list(
      original= m_orig,
      bivariate  = m_bivar,  # may be NULL when not requested or identical to Original
      minimal = if (length(m_mins)) m_mins[[1]] else if (isTRUE(imply)) .safe_fit(engine, .build_minimal_formula(formula, exposure, outcome, minimal), data, engine_args) else m_orig,
      minimal_list = m_mins, # all minimal fits
      canonical = m_canon,
      canonical_excl = m_canon_excl
    ),
    unevaluated = unevaluated,
    unevaluated_str = unevaluated_str,
    verbose = isTRUE(verbose),
    imply = isTRUE(imply)
  )
  
  #compute and store row omit and show info
  report$settings <- list(
    omit_intercept = isTRUE(omit_intercept),
    omit_factors = isTRUE(omit_factors),
    show = show,
    exclude = exclude,
    engine = engine,
    engine_args = engine_args,
    estimand = estimand,
    estimand_requested = estimand_requested,
    weights_args = weights_args,
    auto_acde = isTRUE(auto_acde),
    acde = acde,
    directeffects_args = directeffects_args
  )
  report$.__data <- data
  report$settings$coef_omit <- .build_coef_omit(
    data = report$.__data,
    omit_intercept = report$settings$omit_intercept,
    omit_factors = report$settings$omit_factors
  )
  
  class(report) <- c("DAGassist_report", class(report))
  
  #for console output, do not build exporter objects, as they are computationally 
  #expensive and the console printer will build models later
  mods_full <- NULL
  models_df_full <- NULL
  
  need_export_objects <- !identical(type, "console")
  
  if (isTRUE(need_export_objects)) {
    mods_full <- .build_named_mods(report)
    models_df_full <- .build_models_df(report)
    
    # cache to prevent refitting if the object is printed later.
    report$models_full <- mods_full
    report$models_df_full <- models_df_full
  }
  
  ##### LATEX OUT BRANCH #####
  if (type == "latex") {
    res_min <- list(
      validation = list(
        status = if (isTRUE(v$ok)) "VALID" else "INVALID",
        issues = if (!is.null(v$issues)) v$issues else character(0)
      ),
      coef_rename = labmap,
      coef_omit  = report$settings$coef_omit,
      roles_df = report$roles_display,
      models_df = models_df_full,
      models = mods_full,
      min_sets = report$controls_minimal_all,
      canon = report$controls_canonical,
      unevaluated_str = report$unevaluated_str,
      show = show,
      verbose=verbose
    )
    
    if (!is.null(out)) {
      # only check extension if we actually have a path
      if (tolower(tools::file_ext(out)) == "docx") {
        stop("LaTeX fragment must not be written to a .docx path. Use a .tex filename.", call. = FALSE)
      }
      .report_latex_fragment(res_min, out)
      return(invisible(structure(report, file = normalizePath(out, mustWork = FALSE))))
    } else {
      # print to console instead of erroring
      .report_latex_fragment(res_min, out = NULL)
      return(invisible(report))
    }
  }
  
  ##### WORD OUT BRANCH #####
  if (type %in% c("docx","word")) {
    res_min <- list(
      validation = list(
        status = if (isTRUE(v$ok)) "VALID" else "INVALID",
        issues = if (!is.null(v$issues)) v$issues else character(0)
      ),
      roles_df = report$roles_display,
      coef_omit  = report$settings$coef_omit,
      coef_rename = labmap,
      models = mods_full,                
      min_sets = report$controls_minimal_all,
      canon = report$controls_canonical,
      unevaluated_str = report$unevaluated_str,
      show = show,
      verbose=verbose
    )
    return(.report_docx(res_min, out))
  }
  
  #### EXCEL OUT BRANCH ####
  if (type %in% c("excel","xlsx")) {
    res_min <- list(
      roles_df = report$roles_display,
      coef_omit  = report$settings$coef_omit,
      coef_rename = labmap,
      models = mods_full,         
      min_sets = report$controls_minimal_all,
      canon = report$controls_canonical,
      unevaluated_str = report$unevaluated_str,
      show = show,
      verbose=verbose
    )
    .report_xlsx(res_min, out)
    return(invisible(structure(report, file = normalizePath(out, mustWork = FALSE))))
  }
  
  ##### TEXT OUT BRANCH #####
  if (type %in% c("text","txt")) {
    res_min <- list(
      roles_df = report$roles_display,
      coef_omit  = report$settings$coef_omit,
      coef_rename = labmap,
      models = mods_full,         
      min_sets = report$controls_minimal_all,
      canon = report$controls_canonical,
      unevaluated_str = report$unevaluated_str,
      show = show,
      verbose=verbose
    )
    if (is.null(out)) {
      # console text output support
      .report_txt(res_min, out = NULL)
      return(invisible(report))
    } else {
      .report_txt(res_min, out)
      return(invisible(structure(report, file = normalizePath(out, mustWork = FALSE))))
    }
  }
  
  ##### DOTWHISKER OUT BRANCH #####
  if (type %in% c("dwplot", "dotwhisker")) {
    file_attr <- if (!is.null(out)) normalizePath(out, mustWork = FALSE) else NULL
    .report_dotwhisker(report, out = out)
    return(invisible(structure(report, file = file_attr)))
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
  
  #set show flag for output info
  show <- if (is.null(x$settings$show)) "all" else x$settings$show
  #set verbose flag for suppressing certain parts
  verbose <- if (is.null(x$verbose)) TRUE else isTRUE(x$verbose)
  
  if (show != "models"){
    cat("\nRoles:\n")
    # Only label the variable names; keep the logical flags intact for the S3 printer
    r <- tryCatch(
      .apply_labels_to_roles_df(x$roles_display, x$labels_map),
      error = function(e) x$roles_display
    )
    # ensure the class is still there (usually preserved, but this is harmless)
    if (!inherits(r, "DAGassist_roles")) {
      class(r) <- unique(c("DAGassist_roles", class(r)))
    }
    print(r)  
    
    #only print if there is a formula
    if (!is.null(x$formulas$original)) {
      if (length(x$bad_in_user)) {
        cat(clr_red("\n (!) Bad controls in your formula: {", paste(x$bad_in_user, collapse = ", "), "}\n", sep = ""))
      } else {
        cat(clr_green("\nNo bad controls detected in your formula.\n"))
      }
    }
    #butterfly or m bias
    if (!is.null(x$conditions) && length(x$conditions)) {
      on <- names(Filter(isTRUE, x$conditions))
      if (length(on)) {
        cat(clr_yellow("\nDAG conditions detected: ",
                       paste(on, collapse = ", "),
                       "\n", sep = ""))
      }
    }
    
    # if we're only showing roles (no comparison table), print legend here
    if (!identical(x$settings$show, "all") && !identical(x$settings$show, "models")) {
      if (isTRUE(verbose)) {
        cat(
          "\nRoles legend: Exp. = exposure/treatment; Out. = outcome; CON = confounder; MED = mediator; COL = collider; dOut = descendant of outcome; dMed  = descendant of mediator; dCol = descendant of collider; dConfOn = descendant of a confounder on a back-door path; dConfOff = descendant of a confounder off a back-door path; NCT = neutral control on treatment; NCO = neutral control on outcome\n",
          sep = ""
        )
      } else {
        cat(
          clr_yellow(
            "\nLegend hidden because verbose = FALSE. Re-run with verbose = TRUE to see role definitions.\n"
          )
        )
      }
    }
  }
  if (show != "roles"){
    if (identical(show, "all")){
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
        # bivariate line if present and distinct from Original
        if (!is.null(x$formulas$bivariate)) {
          cat("  bivariate: ", deparse(x$formulas$bivariate), "\n", sep = "")
        }
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
            rhs_i <- setdiff(.rhs_terms_safe(mins_fmls[[i]]), drop_exp)
            added <- setdiff(rhs_i, user_rhs)
            if (length(added)) lines <- c(lines, sprintf("  - Minimal %d added: %s", i, .format_set(added)))
          }
          
          rhs_c <- setdiff(.rhs_terms_safe(x$formulas$canonical), drop_exp)
          added_c <- setdiff(rhs_c, user_rhs)
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
      }
    }
    ## build the model list for modelsummary/broom
    mods <- list("Original" = x$models$original)
    # include Bivariate if present
    if (!is.null(x$models$bivariate)) {
      mods[["Bivariate"]] <- x$models$bivariate
    }
    if (length(x$models$minimal_list)) {
      for (i in seq_along(x$models$minimal_list)) {
        mods[[sprintf("Minimal %d", i)]] <- x$models$minimal_list[[i]]
      }
    } else {
      mods[["Minimal 1"]] <- x$models$minimal
    }
    mods[["Canonical"]] <- x$models$canonical
    
    if (!is.null(x$models$canonical_excl)) {
      if (is.list(x$models$canonical_excl)) {
        for (nm in names(x$models$canonical_excl)) {
          lbl <- switch(
            nm,
            nct = "Canon. (-NCT)",
            nco = "Canon. (-NCO)",
            paste0("Canonical (", nm, ")")
          )
          mods[[lbl]] <- x$models$canonical_excl[[nm]]
        }
      } else {
        # old single-model behavior
        exc <- x$settings$exclude
        lbl <- "Canonical (filtered)"
        if (identical(exc, "nct")) lbl <- "Canon. (-NCT)"
        if (identical(exc, "nco")) lbl <- "Canon. (-NCO)"
        mods[[lbl]] <- x$models$canonical_excl
      }
    }
    
    coef_omit <- x$settings$coef_omit
    
    #build estimand models ONCE. If DAGassist() already cached models_full
    # (e.g., non-console types), reuse to avoid refitting.
    mods_full <- x$models_full
    if (is.null(mods_full) || !is.list(mods_full) || !length(mods_full)) {
      mods_full <- .dagassist_add_estimand_models(x, mods)
    }
    
    #print ACDE diagnostics once, rather than per-model which clutters output
    if (isTRUE(verbose)) {
      .dagassist_print_acde_console_info(mods_full)
    }
    
    .print_model_comparison_list(
      mods_full,
      coef_rename = x$labels_map,
      coef_omit = coef_omit
    )
    #interpretable effects report for weighted estimands
    .dagassist_print_effect_summaries(x, mods_full, only_weighted = TRUE, continuous_scale = "IQR")
    #print weight diagnostics
    if (isTRUE(verbose)) .dagassist_print_weight_diagnostics(mods_full)
    
    if (identical(show, "all")) {
      if (isTRUE(verbose)) {
        cat(
          "\nRoles legend: Exp. = exposure; Out. = outcome; CON = confounder; MED = mediator; COL = collider; dOut = descendant of outcome; dMed  = descendant of mediator; dCol = descendant of collider; dConfOn = descendant of a confounder on a back-door path; dConfOff = descendant of a confounder off a back-door path; NCT = neutral control on treatment; NCO = neutral control on outcome\n",
          sep = ""
        )
      } else {
        cat(
          clr_yellow(
            "\nLegend hidden because verbose = FALSE. Re-run with verbose = TRUE to see role definitions.\n"
          )
        )
      }
    }
  }
}