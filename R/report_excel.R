# R/report_excel.R — minimal, readable, no styling

.report_xlsx <- function(res, out) {
  if (is.null(out) || !nzchar(out)) stop("type='excel' requires `out=`.", call. = FALSE)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Please install {writexl} to export Excel files.", call. = FALSE)
  }
  
  roles <- tryCatch(res$roles_df, error = function(e) NULL)
  mods  <- tryCatch(res$models,    error = function(e) NULL)
  msets <- tryCatch(res$min_sets,  error = function(e) list())
  canon <- tryCatch(res$canon,     error = function(e) character(0))
  # from boolean + raw labels to x grid with standard column nmaes
  df_roles <- .roles_pretty(roles)

  # models sheet from shared helper
  built     <- .build_modelsummary_pretty_df(mods, 
                                             coef_rename=res$coef_rename,
                                             coef_omit = res$coef_omit)
  df_models <- built$df
  
  notes <- c(
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001.",
    paste0("Controls (minimal):   ", if (length(msets)) .set_brace_plain(msets[[1]]) else "{}"),
    paste0("Controls (canonical): ", .set_brace_plain(canon), ".")
  )
  if (!is.null(res$unevaluated_str) && nzchar(res$unevaluated_str)) {
    notes <- c(notes, paste0("Unevaluated regressors (not in DAG): {", res$unevaluated_str, "}"))
  }
  #make the df
  df_notes <- data.frame(
    Notes = notes,
    stringsAsFactors = FALSE
  )
  
  # Collect non-empty sheets (in a stable order)
  sheets <- list()
  if (!is.null(df_roles)  && nrow(df_roles))  sheets$Roles  <- df_roles
  if (!is.null(df_models) && nrow(df_models)) sheets$Models <- df_models
  sheets$Notes <- df_notes
  
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(sheets, path = out)
  invisible(out)
}