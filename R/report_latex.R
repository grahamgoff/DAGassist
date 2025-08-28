############################ INTERNAL HELPERS ##################################

.tex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x, perl = TRUE)
  x <- gsub("([\\{\\}\\$\\#\\%\\_\\&])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, perl = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
  x
}

.df_to_tabular <- function(df) {
  stopifnot(is.data.frame(df))
  df2 <- lapply(df, function(col) .tex_escape(ifelse(is.na(col), "", as.character(col))))
  df2 <- as.data.frame(df2, stringsAsFactors = FALSE, optional = TRUE)
  
  colspec <- paste(rep("l", ncol(df2)), collapse = " ")
  header  <- paste(.tex_escape(colnames(df2)), collapse = " & ")
  body    <- apply(df2, 1L, function(r) paste(r, collapse = " & "))
  
  c(
    paste0("\\begin{tabular}{@{ }", colspec, "@{ }}"),
    "\\hline",
    paste0(header, " \\\\"),
    "\\hline",
    paste0(body, " \\\\"),
    "\\hline",
    "\\end{tabular}"
  )
}

.format_set_tex <- function(s) {
  s <- as.character(s)
  if (!length(s)) return("{}")
  paste0("{", paste(.tex_escape(s), collapse = ", "), "}")
}

################################################################################

# R/report_latex.R
## Minimal LaTeX fragment writer for DAGassist. Produces a PREAMBLE-LESS snippet.
## Include it in your paper with: \input{path/to/fragment.tex}

.report_latex_fragment <- function(res, out) {
  stopifnot(is.character(out), length(out) == 1L)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  
  # grab pieces no matter how 'res' is shaped
  v_ok    <- tryCatch(isTRUE(res$validation$ok), error = function(e) NA)
  v_iss   <- tryCatch(res$validation$issues,     error = function(e) character(0))
  roles   <- tryCatch(
    if (!is.null(res$roles_df)) res$roles_df else res$roles,
    error = function(e) NULL
  )
  formulas <- tryCatch(res$formulas, error = function(e) NULL)
  mins_all <- tryCatch(res$controls_minimal_all, error = function(e) list())
  canon    <- tryCatch(res$controls_canonical,   error = function(e) character(0))
  notes    <- tryCatch(res$notes,                error = function(e) character(0))
  
  status <- if (is.na(v_ok)) "UNKNOWN" else if (v_ok) "VALID" else "INVALID"
  
  # tiny formulas table
  models_df <- NULL
  if (!is.null(formulas)) {
    labs <- c("Original",
              if (length(formulas$minimal_list)) paste0("Minimal ", seq_along(formulas$minimal_list)) else "Minimal 1",
              "Canonical")
    fmts <- c(
      paste(deparse(formulas$original), collapse = " "),
      if (length(formulas$minimal_list))
        vapply(formulas$minimal_list, function(f) paste(deparse(f), collapse = " "), character(1))
      else
        paste(deparse(formulas$minimal), collapse = " "),
      paste(deparse(formulas$canonical), collapse = " ")
    )
    models_df <- data.frame(Model = labs, Formula = fmts, stringsAsFactors = FALSE)
  }
  
  # Adjustment sets section lines
  adj_lines <- character(0)
  if (length(mins_all)) {
    for (i in seq_along(mins_all)) {
      adj_lines <- c(adj_lines, sprintf("Minimal %d: %s", i, .format_set_tex(mins_all[[i]])))
    }
  } else {
    adj_lines <- c(adj_lines, "Minimal 1: {}")
  }
  adj_lines <- c(adj_lines, sprintf("Canonical: %s", .format_set_tex(canon)))
  
  # Build the fragment
  out_lines <- c(
    "% ---- DAGassist LaTeX fragment ----",
    "\\begingroup\\small",
    "\\paragraph{Validation}",
    sprintf("\\textbf{Status:} %s.", .tex_escape(status)),
    if (length(v_iss)) {
      c("\\\\[2pt]\\textbf{Issues:}",
        "\\begin{itemize}",
        paste0("  \\item ", .tex_escape(v_iss)),
        "\\end{itemize}")
    } else {
      "\\\\[2pt]\\textit{No issues reported.}"
    },
    "",
    
    # Roles table (if present)
    if (is.data.frame(roles) && nrow(roles)) {
      c("\\paragraph{Variable roles}",
        .df_to_tabular(roles), "")
    } else character(0),
    
    # Adjustment sets 
    "\\paragraph{Adjustment sets}",
    "\\begin{itemize}",
    paste0("  \\item ", .tex_escape(adj_lines)),
    "\\end{itemize}",
    "",
    
    # tiny formulas table 
    if (is.data.frame(models_df)) {
      c("\\paragraph{Model formulas}",
        .df_to_tabular(models_df), "")
    } else character(0),
    
    # optional notes 
    if (length(notes)) {
      c("\\paragraph{Notes}",
        paste0("\\emph{", .tex_escape(notes), "}"), "")
    } else character(0),
    
    "\\endgroup"
  )
  
  writeLines(out_lines, out, useBytes = TRUE)
  invisible(out)
}

