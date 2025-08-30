
# Brace-formatter used in Notes / sets
.set_brace_plain <- function(s) {
  s <- as.character(s); if (!length(s)) return("{}")
  paste0("{", paste(s, collapse = ", "), "}")
}

# Prefer your own pretty roles helper if present
.roles_pretty_for_xl <- function(roles) {
  if (exists(".roles_pretty", mode = "function")) return(.roles_pretty(roles))
  roles
}

# Prefer your stacked-SE builder if present; otherwise quick fallback
.build_modelsummary_pretty_df_xl <- function(mods) {
  if (exists(".build_modelsummary_pretty_df", mode = "function")) {
    out <- .build_modelsummary_pretty_df(mods); return(out$df)
  }
  # Minimal fallback (no stacking) to avoid hard fail if helper missing
  if (!requireNamespace("modelsummary", quietly = TRUE)) return(NULL)
  df <- modelsummary::msummary(
    mods, output = "data.frame", stars = TRUE,
    gof_omit = "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma"
  )
  if (!is.data.frame(df) || !nrow(df)) return(NULL)
  df[] <- lapply(df, function(x) { y <- as.character(x); y[is.na(y)] <- ""; y })
  meta  <- intersect(c("part","term","statistic"), names(df))
  keep  <- setdiff(names(df), meta)
  est   <- df[df$part == "estimates" & df$statistic == "estimate", c("term", keep), drop = FALSE]
  names(est)[1] <- "Term"
  est
}

# Adjustment sets -> tidy df
.min_sets_to_df <- function(min_sets, canon) {
  if (!length(min_sets) && !length(canon)) {
    return(data.frame(Set = character(0), Controls = character(0)))
  }
  rows <- list()
  if (length(min_sets)) {
    for (i in seq_along(min_sets)) {
      rows[[length(rows) + 1L]] <- data.frame(
        Set = paste0("Minimal ", i),
        Controls = .set_brace_plain(min_sets[[i]]),
        stringsAsFactors = FALSE
      )
    }
  } else {
    rows[[length(rows) + 1L]] <- data.frame(Set = "Minimal 1", Controls = "{}", stringsAsFactors = FALSE)
  }
  rows[[length(rows) + 1L]] <- data.frame(Set = "Canonical", Controls = .set_brace_plain(canon), stringsAsFactors = FALSE)
  do.call(rbind, rows)
}

# MAIN: write an .xlsx file with sheets: Roles, Models, Notes, (optional) Sets
# Expects `res` with: $roles_df, $models, $min_sets, $canon
.report_xlsx <- function(res, out) {
  # Prefer openxlsx for styling; fall back to writexl if missing
  has_openxlsx <- requireNamespace("openxlsx", quietly = TRUE)
  has_writexl  <- requireNamespace("writexl",  quietly = TRUE)
  
  roles <- tryCatch(res$roles_df, error = function(e) NULL)
  mods  <- tryCatch(res$models,    error = function(e) NULL)
  msets <- tryCatch(res$min_sets,  error = function(e) list())
  canon <- tryCatch(res$canon,     error = function(e) character(0))
  
  # Build the data frames
  df_roles  <- if (is.data.frame(roles) && nrow(roles)) .roles_pretty_for_xl(roles) else NULL
  df_models <- if (!is.null(mods)) .build_modelsummary_pretty_df_xl(mods) else NULL
  df_sets   <- .min_sets_to_df(msets, canon)
  df_notes  <- data.frame(
    Item  = c("p-value legend", "Controls (minimal)", "Controls (canonical)"),
    Value = c("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001.",
              if (length(msets)) .set_brace_plain(msets[[1]]) else "{}",
              .set_brace_plain(canon)),
    stringsAsFactors = FALSE
  )
  
  # Ensure directory
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  
  # --- Styling path (openxlsx) ---
  if (has_openxlsx) {
    wb <- openxlsx::createWorkbook()
    
    add_sheet <- function(name, df, freeze = TRUE) {
      if (is.null(df) || !nrow(df)) return(invisible(NULL))
      openxlsx::addWorksheet(wb, name)
      openxlsx::writeData(wb, name, df, withFilter = TRUE)
      # Styles
      hdr <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center")
      bdy <- openxlsx::createStyle(halign = "center", valign = "center", wrapText = TRUE)
      left <- openxlsx::createStyle(halign = "left", valign = "center", wrapText = TRUE)
      # Header style
      openxlsx::addStyle(wb, name, hdr, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
      # Body style (center everything except first column)
      if (ncol(df) >= 1) {
        if (ncol(df) > 1) openxlsx::addStyle(wb, name, bdy, rows = 2:(nrow(df)+1), cols = 2:ncol(df), gridExpand = TRUE)
        openxlsx::addStyle(wb, name, left, rows = 2:(nrow(df)+1), cols = 1, gridExpand = TRUE)
      }
      # Borders & column widths
      openxlsx::addStyle(wb, name, openxlsx::createStyle(border = "bottom"), rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE, stack = TRUE)
      openxlsx::setColWidths(wb, name, cols = seq_len(ncol(df)), widths = "auto")
      if (freeze) openxlsx::freezePane(wb, name, firstActiveRow = 2, firstActiveCol = 2)
    }
    
    add_sheet("Roles",  df_roles)
    add_sheet("Models", df_models)
    add_sheet("Sets",   df_sets)
    add_sheet("Notes",  df_notes, freeze = FALSE)
    
    openxlsx::saveWorkbook(wb, out, overwrite = TRUE)
    return(invisible(out))
  }
  
  # --- Minimal path (writexl) ---
  if (has_writexl) {
    sheets <- list()
    if (!is.null(df_roles))  sheets$Roles  <- df_roles
    if (!is.null(df_models)) sheets$Models <- df_models
    if (nrow(df_sets))       sheets$Sets   <- df_sets
    sheets$Notes <- df_notes
    writexl::write_xlsx(sheets, path = out)
    return(invisible(out))
  }
  
  stop("Please install either {openxlsx} (recommended) or {writexl} to export Excel files.", call. = FALSE)
}