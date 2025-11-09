# R/report_text.R — minimal & readable

.report_txt <- function(res, out) {

  roles <- tryCatch(res$roles_df, error = function(e) NULL)
  mods  <- tryCatch(res$models,    error = function(e) NULL)
  msets <- tryCatch(res$min_sets,  error = function(e) list())
  canon <- tryCatch(res$canon,     error = function(e) character(0))
  cmap  <- tryCatch(res$coef_rename, error = function(e) NULL)  
  show <- tryCatch(res$show, error = function(e) "all")
  
  lines <- c("DAGassist Report:", "")
  
  #use pretty roles x grid if available. else, print bool stacks
  if (show != "models" && is.data.frame(roles) && nrow(roles)) {
    rp <- if (exists(".roles_pretty", mode = "function")) .roles_pretty(roles) else roles
    lines <- c(lines, "## Roles", "", .df_to_md_pipe(rp), "")
  }
  
  if(show!= "roles" && !is.null(mods)){
    #standard stacked models 
    built <- .build_modelsummary_pretty_df(mods, 
                                           coef_rename = cmap,
                                           coef_omit = res$coef_omit)
    if (!is.null(built$df) && nrow(built$df)) {
      lines <- c(lines, "## Models", "", .df_to_md_pipe(built$df), "")
    }
  }

  
  #make notes
  notes <- c("p-value legend: + < 0.1, * < 0.05, ** < 0.01, *** < 0.001.")
  if (show != "roles") {
    notes <- c(
      notes,
      sprintf("Controls (minimal): %s.", if (length(msets)) .set_brace_plain(msets[[1]]) else "{}"),
      sprintf("Controls (canonical): %s.", .set_brace_plain(canon))
    )
    if (!is.null(res$unevaluated_str) && nzchar(res$unevaluated_str)) {
      notes <- c(notes, sprintf("Unevaluated regressors (not in DAG): {%s}.", res$unevaluated_str))
    }
  }
  
  lines <- c(lines, "### Notes", "", paste0("- ", notes), "")
  
  if (is.null(out)) {
    cat(paste(lines, collapse = "\n"), "\n")
    invisible(NULL)
  } else {
    dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, out, useBytes = TRUE)
    invisible(out)
  }
}