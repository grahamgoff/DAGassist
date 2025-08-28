############################ INTERNAL HELPERS ##################################
# Convert tinytable's tabularray (talltblr) to a booktabs longtable.
.talltblr_to_longtable <- function(x) {
  # How many columns? Parse colspec={Q[]Q[]...}
  cs <- regmatches(x, regexpr("colspec=\\{[^}]*\\}", x, perl = TRUE))
  n  <- 3L
  if (length(cs) && nzchar(cs)) {
    m <- gregexpr("Q\\[\\]", cs[[1]], perl = TRUE)[[1]]
    if (length(m) && m[1] != -1L) n <- length(m)
  }
  colspec <- paste0("@{ }", paste(rep("l", n), collapse = " "), "@{ }")
  
  # Replace talltblr header with longtable header
  x <- gsub("\\\\begin\\{talltblr\\}\\[[^\\]]*\\]\\s*\\{[^}]*\\}\\s*",
            paste0("\\\\begin{longtable}{", colspec, "}\n"),
            x, perl = TRUE)
  
  # Drop tabularray-only config lines (notes/column/hline config)
  x <- gsub("^.*note\\{\\}\\=.*\\n", "", x, perl = TRUE)
  x <- gsub("^column\\{[^}]*\\}\\=\\{\\}\\{[^}]*\\},?\\s*$", "", x, perl = TRUE)
  x <- gsub("^hline\\{.*\\}.*\\n", "", x, perl = TRUE)
  
  # Close environment
  x <- gsub("\\\\end\\{talltblr\\}", "\\\\end{longtable}", x, perl = TRUE)
  
  # Strip any siunitx remnants for safety
  x <- gsub("\\\\num\\{([^}]*)\\}", "\\1", x, perl = TRUE)
  x
}

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

  if ("Term" %in% colnames(df2)) {
    df2[["Term"]] <- ifelse(nzchar(df2[["Term"]]),
                            paste0("\\textnormal{", df2[["Term"]], "}"),
                            df2[["Term"]])
  }
  df2[] <- lapply(df2, function(col) ifelse(nzchar(col), paste0("{", col, "}"), col))
  
  if (wrap_formula && "Formula" %in% colnames(df2) && ncol(df2) >= 2) {
    mid <- max(0, ncol(df2) - 2)
    colspec <- paste0(
      "@{}l",
      if (mid) paste(rep(" l", mid), collapse = ""),
      " p{.68\\textwidth}@{}"   # narrower to fit page
    )
  } else {
    colspec <- paste0("@{}", paste(rep("l", ncol(df2)), collapse = " "), "@{}")  # no side padding
  }
  
  header  <- paste(.tex_escape(colnames(df2)), collapse = " & ")
  body    <- apply(df2, 1L, function(r) paste(r, collapse = " & "))
  
  c(
    "\\begin{center}",
    # LT margins once per table is fine (also set globally below)
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

# Minimal, pretty model comparison: "coef (se)" in one cell, GOF at bottom.
.msummary_to_longtable_centered <- function(mods) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    return(c("% modelsummary not installed; skipping model comparison"))
  }
  
  df <- modelsummary::msummary(
    mods,
    output   = "data.frame",
    stars    = TRUE,
    gof_omit = "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma"
  )
  if (!is.data.frame(df) || nrow(df) == 0L) return(c("% empty modelsummary table"))
  
  # Make everything character; fill NAs with ""
  df[] <- lapply(df, function(x) { y <- as.character(x); y[is.na(y)] <- ""; y })
  
  meta        <- intersect(c("part","term","statistic"), names(df))
  model_cols  <- setdiff(names(df), meta)
  if (!length(model_cols)) return(c("% no model columns"))
  
  est <- df[df$part == "estimates", c("term","statistic", model_cols), drop = FALSE]
  gof <- df[df$part == "gof",       c("term",            model_cols), drop = FALSE]
  
  est_est <- est[est$statistic == "estimate",   , drop = FALSE]
  est_se  <- est[est$statistic == "std.error",  , drop = FALSE]
  
  # Keep the original order of terms as in estimates
  terms <- est_est$term
  pretty <- data.frame(Term = terms, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Build coef (se) per model column (strip any preexisting SE parentheses 
  # because double parinth trips latex)
  for (mc in model_cols) {
    coef_vals <- est_est[[mc]]
    se_vals   <- est_se [match(terms, est_se$term), mc]
    
    coef_vals <- ifelse(is.na(coef_vals), "", coef_vals)
    # remove any '(' or ')' already present, then wrap once
    se_clean  <- ifelse(is.na(se_vals) | !nzchar(se_vals), "", gsub("[()]", "", se_vals))
    
    pretty[[mc]] <- ifelse(nzchar(se_clean),
                           paste0(coef_vals, " (", se_clean, ")"),
                           coef_vals)
  }
  
  # Append blank line and GOF rows (if any)
  if (nrow(gof)) {
    blank <- data.frame(Term = "", as.list(rep("", length(model_cols))),
                        check.names = FALSE, stringsAsFactors = FALSE)
    names(blank)[-1] <- model_cols
    names(gof)[1] <- "Term"
    pretty <- rbind(pretty, blank, gof)
  }
  
  # Print as centered longtable with booktabs
  c(
    .df_to_longtable_centered(pretty),
    "{\\footnotesize \\emph{Notes:} + p $< 0.1$, * p $< 0.05$, ** p $< 0.01$, *** p $< 0.001$.}"
  )
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
  
  # Build LaTeX lines (lean layout; no validation; no adj-sets table)
  lines <- c(
    "% ---- DAGassist LaTeX fragment (no preamble) ----",
    "% Requires: \\usepackage{longtable}, \\usepackage{booktabs}",
    "\\begingroup\\footnotesize",
    # compact layout for publication
    "\\setlength{\\LTleft}{0pt}\\setlength{\\LTright}{0pt}",
    "\\setlength{\\tabcolsep}{4pt}",
    "\\renewcommand{\\arraystretch}{0.95}",
    "\\setlength{\\aboverulesep}{0pt}\\setlength{\\belowrulesep}{0pt}",
    
    # --- Variable roles (keep) ---
    if (is.data.frame(roles) && nrow(roles)) {
      roles_pretty <- .roles_pretty(roles)
      c("\\noindent\\textbf{Variable roles}",
        .df_to_longtable_centered(roles_pretty),
        # one-line controls summary instead of a whole table
        {
          ctrl_min <- if (length(msets)) .set_brace(msets[[1]]) else "{}"
          ctrl_can <- if (length(canon)) .set_brace(canon) else "{}"
          paste0("\\noindent\\textit{Controls:} Minimal ", ctrl_min,
                 " \\quad Canonical ", ctrl_can, "\n")
        })
    } else character(0),
    
    # --- Model formulas (keep, compact) ---
    if (is.data.frame(f_tbl) && nrow(f_tbl)) {
      colnames(f_tbl) <- c("Model", "Formula")
      c("\\noindent\\textbf{Model formulas}",
        .df_to_longtable_centered(f_tbl, wrap_formula = TRUE))
    } else character(0),
    
    # --- Model comparison (keep) ---
    if (!is.null(mods)) {
      c("\\noindent\\textbf{Model comparison}",
        .msummary_to_longtable_centered(mods),
        "{\\footnotesize \\emph{Notes:} + p $< 0.1$, * p $< 0.05$, ** p $< 0.01$, *** p $< 0.001$.}")
    } else character(0),
    
    "\\endgroup"
  )
  
  writeLines(unlist(lines), out, useBytes = TRUE)
  invisible(out)
}