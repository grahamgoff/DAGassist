#' Validate dag, formula, data before fitting and ensure exposure and outcome are
#' specified
#' 
#' @param dag A `dagitty` DAG object.
#' @param formula a regression call
#' @param data data frame
#' @param exposure Character; exposure node name (X).
#' @param outcome Character; outcome node name (Y).
#'
#' @return A list of class `DAGassist_validation` with values:  
#' `ok` (logical), `issues` (data.frame), `vars` (list)
#'
#' @examples
#' # Load small demo DAG and data shipped with the package
#' data(test_complex, package = "DAGassist")
#' data(test_df,      package = "DAGassist")
#'
#' # example of what to do: everything present and consistent
#' v_ok <- validate_spec(test_complex, Y ~ X + Z, test_df,
#'                       exposure = "X", outcome = "Y")
#' v_ok
#'
#' ##common mistake: a variable in the formula is missing from the data
#' partial_df <- test_df[c("Y", "X")]
#' v_missing <- validate_spec(test_complex, Y ~ X + Z, partial_df,
#'                            exposure = "X", outcome = "Y")
#' v_missing
#'
#' ##another common issue: exposure not on the right-hand side (warns)
#' v_warn <- validate_spec(test_complex, Y ~ Z, test_df,
#'                         exposure = "X", outcome = "Y")
#' v_warn
#'
#' # 4) (Not run) Example that would error: DAG is not a dagitty object
#' # \dontrun{
#' #   validate_spec(list(), Y ~ X, test_df, exposure = "X", outcome = "Y")
#' # }
#' 
#' @export

validate_spec <- function(dag, formula, data, exposure, outcome){
  
  # Capture the original expression the user passed for better error messages
  data_expr  <- substitute(data)           # the unevaluated expression
  data_label <- deparse(data_expr, nlines = 1)
  
  # standardize inputs as formulas
  if(is.character(formula)) formula <- stats::as.formula(formula)
  # let the user know if the inputs do not work
  if(!inherits(dag, "dagitty")){
    stop("`dag` must be a dagitty object. Create it with dagitty::dagitty() or 
         ggdag::dagify().")
  }
  
  # if data is a character, look it up for helpful error message
  if (is.character(data)) {
    # search the environment for an object with that name
    data_resolved <- get0(data, envir = parent.frame(), inherits = TRUE)
    if (is.null(data_resolved)) {
      stop(sprintf("No object called '%s' found in your environment.", data), 
           call. = FALSE)
    }
    data <- data_resolved
  }
  
  # if there is not a data argument
  if (missing(data)) {
    stop("`data` argument is missing, with no default.")
  }
  
  # only accept data.frame or tibble -- let user know what is wrong
  if (!is.data.frame(data)) {
    stop(sprintf("`data` must be a data.frame or tibble; got <%s>.",
                 paste(class(data), collapse = "/")))
  }
  data <- as.data.frame(data) #convert tibbles

  #collect variable names
  dag_vars <- names(dag)
  formula_vars <- all.vars(formula)
  data_vars <- names(data)
  
  #create issues table which we will append to as we go
  issues <- new_issue_table() 
  # Local helper to add a row to the issues table (mutates `issues` via <<-)
  add_issue <- function(severity, type, variable, message) {
    issues <<- rbind(
      issues, data.frame(severity = severity, type = type, variable = variable, 
                         message = message, stringsAsFactors = FALSE))
  }
  
  #add entries to issue table if the exposure or outcome are incorrect
  if (!exposure %in% dag_vars)  add_issue("error", "missing_in_dag",  exposure, "Exposure not found in DAG.")
  if (!outcome  %in% dag_vars)  add_issue("error", "missing_in_dag",  outcome,  "Outcome not found in DAG.")
  if (!exposure %in% data_vars) add_issue("error", "missing_in_data", exposure, "Exposure not found in data.")
  if (!outcome  %in% data_vars) add_issue("error", "missing_in_data", outcome,  "Outcome not found in data.")
  
  #add entries to issue table if there are variables in the formula that are not present in the data or DAG
  for (v in setdiff(formula_vars, dag_vars))  add_issue("error", "missing_in_dag",  v, "Variable in formula not found in DAG.")
  for (v in setdiff(formula_vars, data_vars)) add_issue("error", "missing_in_data", v, "Variable in formula not found in data.")
  
  ## check the formula structure
  
  # is formula DV the same as the `outcome` argument?
  formula_response <- formula_vars[1]
  # RHS labels (the terms after the ~)
  formula_rhs <- attr(stats::terms(formula), "term.labels")
  
  if (!identical(formula_response, outcome)) {
    add_issue(
      "warn", "formula_outcome_mismatch", outcome,
      sprintf("Formula outcome is '%s', but `outcome` argument is '%s'.", formula_response, outcome)
    )
  }
  # warn if the exposure isn't explicitly on the RHS
  if (!exposure %in% formula_rhs) {
    add_issue("warn", "formula_exposure_missing", exposure, "Exposure is not on the right-hand side of the formula.")
  }
  
  # make result object
  out <- list(
    ok              = !any(issues$severity == "error"),
    issues          = issues,
    vars            = list(dag_vars = dag_vars, data_vars = data_vars, formula_vars = formula_vars)
    )
  class(out) <- c("DAGassist_validation", class(out))
  out
}

#' Minimal, clean printout for validation results with color coding
#' @param x the list (class `out`) from validate_spec
#' @param n Max number of issues to show (default 10).
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
print.DAGassist_validation <- function(x, n = 10, ...) {
  # tiny helper: color if cli is available, otherwise plain
  has_cli <- requireNamespace("cli", quietly = TRUE)
  col <- function(txt, color = c("green","red","yellow")) {
    color <- match.arg(color)
    if (!has_cli) return(txt)
    switch(color,
           green  = cli::col_green(txt),
           red    = cli::col_red(txt),
           yellow = cli::col_yellow(txt)
    )
  }
  
  # print errors red
  status <- if (isTRUE(x$ok)) col("VALID", "green") else col("INVALID", "red")
  cat("DAGassist validation: ", status, "\n", sep = "")
  
  # print valid green
  if (!nrow(x$issues)) {
    cat("No issues detected.\n")
    return(invisible(x))
  }
  
  # count the errors and warnings
  n_err  <- sum(x$issues$severity == "error")
  n_warn <- sum(x$issues$severity == "warn")
  cat(sprintf("Issues: %s error(s), %s warning(s)\n",
              if (n_err) col(as.character(n_err), "red") else "0",
              if (n_warn) col(as.character(n_warn), "yellow") else "0"))
  
  # print errors first, then warning, then sort by variable and type
  ord <- order(factor(x$issues$severity, levels = c("error","warn")),
               x$issues$variable, x$issues$type)
  iss <- x$issues[ord, , drop = FALSE]
  
  # order with bullet points
  show_idx <- seq_len(min(n, nrow(iss)))
  for (i in show_idx) {
    sev <- if (iss$severity[i] == "error") col("[error]", "red") else col("[warn]", "yellow")
    line <- sprintf("- %s %s - %s", sev, iss$variable[i], iss$message[i])
    if (nzchar(iss$type[i])) line <- paste0(line, " (", iss$type[i], ")")
    cat(line, "\n")
  }
  if (nrow(iss) > n) cat(sprintf("...and %d more\n", nrow(iss) - n))
  
  invisible(x)
}


# non exported helper function to ensure formatting and struecute consistency
new_issue_table <- function() {
  data.frame(
    severity = character(),  # error or warn
    type     = character(),  # error type: like "missing_in_data" 
    variable = character(),  # problematic variable
    message  = character(),  # readable explanation of the problem
    stringsAsFactors = FALSE
  )
}