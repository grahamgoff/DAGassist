############################ INTERNAL HELPERS ##################################

.tex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x, perl = TRUE)
  x <- gsub("([\\{\\}\\$\\#\\%\\_\\&])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, perl = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
  x
}

.roles_pretty <- function(roles) {
  r <- roles
  map <- c(
    variable = "Variable",
    role     = "Role",
    is_exposure = "X",
    is_outcome  = "Y",
    is_confounder = "Conf.",
    is_mediator   = "Med.",
    is_collider   = "Col.",
    is_descendant_of_outcome = "Desc(Y)",
    is_descendant_of_exposure = "Desc(X)",
    canon = "Canon"
  )
  prefer <- c("variable","role","is_exposure","is_outcome","is_confounder",
              "is_mediator","is_collider","is_descendant_of_outcome",
              "is_descendant_of_exposure","canon")
  keep <- intersect(prefer, names(r))
  r <- r[, c(keep, setdiff(names(r), keep)), drop = FALSE]
  for (nm in names(r)) if (is.logical(r[[nm]])) r[[nm]] <- ifelse(r[[nm]], "x", "")
  names(r) <- ifelse(names(r) %in% names(map), unname(map[names(r)]), names(r))
  r
}

# Centered longtable. If wrap_formula=TRUE and a "Formula" column exists, wrap it.
.df_to_longtable_centered <- function(df, wrap_formula = FALSE) {
  stopifnot(is.data.frame(df))
  df2 <- lapply(df, function(col) .tex_escape(ifelse(is.na(col), "", as.character(col))))
  df2 <- as.data.frame(df2, stringsAsFactors = FALSE, optional = TRUE)
  
  if (wrap_formula && "Formula" %in% colnames(df2) && ncol(df2) >= 2) {
    # first col 'l', last col 'p{.72\\textwidth}', any middle cols 'l'
    mid <- max(0, ncol(df2) - 2)
    colspec <- paste0(
      "@{ }l ",
      if (mid) paste(rep("l ", mid), collapse = ""),
      "p{.72\\textwidth}@{ }"
    )
  } else {
    colspec <- paste0("@{ }", paste(rep("l", ncol(df2)), collapse = " "), "@{ }")
  }
  
  header  <- paste(.tex_escape(colnames(df2)), collapse = " & ")
  body    <- apply(df2, 1L, function(r) paste(r, collapse = " & "))
  
  c(
    "\\begin{center}",
    "\\setlength{\\LTleft}{0pt}\\setlength{\\LTright}{0pt}",
    paste0("\\begin{longtable}{", colspec, "}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    paste0(body, " \\\\"),
    "\\bottomrule",
    "\\end{longtable}",
    "\\end{center}"
  )
}

.sets_to_df <- function(min_sets, canon) {
  if (length(min_sets) == 0 && length(canon) == 0) {
    return(data.frame(Set = character(0), Controls = character(0)))
  }
  rows <- list()
  if (length(min_sets)) {
    for (i in seq_along(min_sets)) {
      rows[[length(rows)+1]] <- data.frame(
        Set = paste0("Minimal ", i),
        Controls = .set_brace(min_sets[[i]]),
        stringsAsFactors = FALSE
      )
    }
  } else {
    rows[[length(rows)+1]] <- data.frame(Set = "Minimal 1", Controls = "{}", stringsAsFactors = FALSE)
  }
  rows[[length(rows)+1]] <- data.frame(Set = "Canonical", Controls = .set_brace(canon), stringsAsFactors = FALSE)
  do.call(rbind, rows)
}

.set_brace <- function(s) {
  s <- as.character(s)
  if (!length(s)) return("{}")
  paste0("{", paste(.tex_escape(s), collapse = ", "), "}")
}

# Render modelsummary to plain LaTeX tabular, then convert to longtable & center.
.msummary_to_longtable_centered <- function(mods) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    return(c("% modelsummary not installed; skipping model comparison"))
  }
  lat <- tryCatch(
    modelsummary::msummary(
      mods,
      output   = "latex_tabular",  # <- avoids tabularray/tinytable
      stars    = TRUE,
      gof_omit = "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma"
    ),
    error = function(e) NULL
  )
  if (is.null(lat)) return(c("% modelsummary failed; skipping"))
  if (is.character(lat)) lat <- paste(lat, collapse = "\n")
  
  # If any \num{} remain, strip them (avoid siunitx dependency)
  lat <- gsub("\\\\num\\{([^}]*)\\}", "\\1", lat, perl = TRUE)
  
  # Remove floating table wrappers if present
  lat <- gsub("\\\\begin\\{table\\*?}[^\n]*\n", "", lat, perl = TRUE)
  lat <- gsub("\\\\end\\{table\\*?}", "", lat, perl = TRUE)
  lat <- gsub("\\\\caption\\{[^}]*\\}", "", lat, perl = TRUE)
  lat <- gsub("\\\\label\\{[^}]*\\}",   "", lat, perl = TRUE)
  
  # Convert tabular -> longtable
  lat <- gsub("\\\\begin\\{tabular\\}", "\\\\begin{longtable}", lat)
  lat <- gsub("\\\\end\\{tabular\\}",   "\\\\end{longtable}",   lat)
  
  c("\\begin{center}",
    "\\setlength{\\LTleft}{0pt}\\setlength{\\LTright}{0pt}",
    lat,
    "\\end{center}")
}

################################################################################

# R/report_latex.R
## Minimal LaTeX fragment writer for DAGassist. Produces a PREAMBLE-LESS snippet.
## Include it in your paper with: \input{path/to/fragment.tex}
## Pretty LaTeX fragment: centered longtables, tidy headers, adj sets, modelsummary

.report_latex_fragment <- function(res, out) {
  stopifnot(is.character(out), length(out) == 1L)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  
  status <- tryCatch(res$validation$status, error = function(e) "UNKNOWN")
  issues <- tryCatch(res$validation$issues, error = function(e) character(0))
  if (is.list(issues)) issues <- unlist(issues, use.names = FALSE)
  issues <- issues[nzchar(issues)]
  
  roles  <- tryCatch(res$roles_df, error = function(e) NULL)
  f_tbl  <- tryCatch(res$models_df, error = function(e) NULL)
  mods   <- tryCatch(res$models,    error = function(e) NULL)
  msets  <- tryCatch(res$min_sets,  error = function(e) list())
  canon  <- tryCatch(res$canon,     error = function(e) character(0))
  
  lines <- c(
    "% ---- DAGassist LaTeX fragment (no preamble) ----",
    "% Requires: \\usepackage{longtable}, \\usepackage{booktabs}",
    "\\begingroup\\small",
    
    # Validation
    "\\paragraph{Validation}",
    sprintf("\\textbf{Status:} %s.", .tex_escape(status)),
    if (length(issues)) {
      c("\\\\[2pt]\\textbf{Issues:}",
        "\\begin{itemize}",
        paste0("  \\item ", .tex_escape(issues)),
        "\\end{itemize}")
    } else "\\\\[2pt]\\textit{No issues reported.}",
    
    "",
    
    # Roles
    if (is.data.frame(roles) && nrow(roles)) {
      roles_pretty <- .roles_pretty(roles)
      c("\\paragraph{Variable roles}",
        .df_to_longtable_centered(roles_pretty), "")
    } else character(0),
    
    # Adjustment sets
    {
      adj_df <- .sets_to_df(msets, canon)
      if (nrow(adj_df)) {
        c("\\paragraph{Adjustment sets}",
          .df_to_longtable_centered(adj_df), "")
      } else character(0)
    },
    
    # Model formulas (wrap the Formula column)
    if (is.data.frame(f_tbl) && nrow(f_tbl)) {
      colnames(f_tbl) <- c("Model", "Formula")
      c("\\paragraph{Model formulas}",
        .df_to_longtable_centered(f_tbl, wrap_formula = TRUE), "")
    } else character(0),
    
    # Model comparison via modelsummary
    if (!is.null(mods)) {
      c("\\paragraph{Model comparison}",
        .msummary_to_longtable_centered(mods), "")
    } else character(0),
    
    "\\endgroup"
  )
  
  writeLines(unlist(lines), out, useBytes = TRUE)
  invisible(out)
}