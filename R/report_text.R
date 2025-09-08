# R/report_text.R — minimal & readable

.report_txt <- function(res, out) {
  if (is.null(out) || !nzchar(out)) stop("type='text' requires `out=`.", call. = FALSE)
  
  roles <- tryCatch(res$roles_df, error = function(e) NULL)
  mods  <- tryCatch(res$models,    error = function(e) NULL)
  msets <- tryCatch(res$min_sets,  error = function(e) list())
  canon <- tryCatch(res$canon,     error = function(e) character(0))
  cmap  <- tryCatch(res$coef_rename, error = function(e) NULL)  
  
  lines <- c("DAGassist Report:", "")
  
  #use pretty roles x grid if available. else, print bool stacks
  if (is.data.frame(roles) && nrow(roles)) {
    rp <- if (exists(".roles_pretty", mode = "function")) .roles_pretty(roles) else roles
    lines <- c(lines, "## Roles", "", .df_to_md_pipe(rp), "")
  }
  
  #standard stacked models 
  built <- .build_modelsummary_pretty_df(mods, 
                                         coef_rename = cmap,
                                         coef_omit = res$coef_omit)
  if (!is.null(built$df) && nrow(built$df)) {
    lines <- c(lines, "## Models", "", .df_to_md_pipe(built$df), "")
  }
  
  #make notes
  notes <- c(
    "## Notes",
    "",
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001.",
    paste0("*Controls (minimal):* ",
           if (length(msets)) .set_brace_plain(msets[[1]]) else "{}"),
    paste0("*Controls (canonical):* ", .set_brace_plain(canon), ".")
  )
  if (!is.null(res$unevaluated_str) && nzchar(res$unevaluated_str)) {
    notes <- c(notes,
               paste0("*Unevaluated regressors (not in DAG):* {", res$unevaluated_str, "}"))
  }
  lines <- c(lines, notes, "")
  
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, out, useBytes = TRUE)
  invisible(out)
}