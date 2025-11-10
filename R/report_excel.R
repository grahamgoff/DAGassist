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
  show <- tryCatch(res$show, error = function(e) "all")
  
  # from boolean + raw labels to x grid with standard column nmaes
  df_roles <- .roles_pretty(roles)

  # models sheet from shared helper
  built     <- .build_modelsummary_pretty_df(mods, 
                                             coef_rename=res$coef_rename,
                                             coef_omit = res$coef_omit)
  df_models <- built$df
  
  if (identical(show, "models")) df_roles  <- df_roles[0, , drop = FALSE]
  if (identical(show, "roles"))  df_models <- df_models[0, , drop = FALSE]
  
  notes <- c("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001.")
  if (show != "roles") {
    notes <- c(notes,
               paste0("Controls (minimal):   ", if (length(msets)) .set_brace_plain(msets[[1]]) else "{}"),
               paste0("Controls (canonical): ", .set_brace_plain(canon), "."))
    if (!is.null(res$unevaluated_str) && nzchar(res$unevaluated_str))
      notes <- c(notes, paste0("Unevaluated regressors (not in DAG): {", res$unevaluated_str, "}"))
  }
  
  sheets <- list()
  if (!is.null(df_roles)  && nrow(df_roles))  sheets$Roles  <- df_roles
  if (!is.null(df_models) && nrow(df_models)) sheets$Models <- df_models
  sheets$Notes <- data.frame(Notes = notes, stringsAsFactors = FALSE)
  
  if (isTRUE(res$verbose) && show != "models") {
    notes <- c(
      "Roles legend: X (exposure); Y (outcome); CON (confounder); MED (mediator); COL (collider); dOut (proper descendant of Y); dMed (proper descendant of any mediator); dCol (proper descendant of any collider); dConfOn (descendant of a confounder on a back-door path); dConfOff (descendant of a confounder off a back-door path); NCT (neutral control on treatment); NCO (neutral control on outcome).",
      notes
    )
  }
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(sheets, path = out)
  invisible(out)
}