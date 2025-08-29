############################ INTERNAL HELPERS ##################################
#force same width between df and modelsummary, to make them look like a single 
#block
.equal_width_colspec <- function(n) {
  # total column count n; keep default \tabcolsep between columns
  w <- sprintf("p{\\dimexpr(\\textwidth - %d\\tabcolsep)/%d\\relax}", 2L*(n-1L), n)
  # no @{} between columns; only trim outer padding so left/right edges align
  paste0("@{}", paste(rep(w, n), collapse = ""), "@{}")
}

# Convert tinytable's tabularray (talltblr) to a booktabs longtable.
.talltblr_to_longtable <- function(x) {
  # How many columns? Parse colspec={Q[]Q[]...}
  cs <- regmatches(x, regexpr("colspec=\\{[^}]*\\}", x, perl = TRUE))
  n  <- 3L
  if (length(cs) && nzchar(cs)) {
    m <- gregexpr("Q\\[\\]", cs[[1]], perl = TRUE)[[1]]
    if (length(m) && m[1] != -1L) n <- length(m)
  }
  #standard column width
  colspec <- .equal_width_colspec(n)
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
# NEW: wrap_center=TRUE by default, set FALSE to return only the longtable.
# Centered longtable. If wrap_formula=TRUE and a "Formula" column exists, wrap it.
# NEW: raw_cols = character() to mark columns that should NOT be escaped.
.df_to_longtable_centered <- function(df, wrap_formula = FALSE, wrap_center = TRUE, raw_cols = character()) {
  stopifnot(is.data.frame(df))
  df2 <- df
  for (nm in names(df2)) {
    if (!(nm %in% raw_cols)) {
      df2[[nm]] <- .tex_escape(ifelse(is.na(df2[[nm]]), "", as.character(df2[[nm]])))
    } else {
      df2[[nm]] <- ifelse(is.na(df2[[nm]]), "", as.character(df2[[nm]]))
    }
  }
  df2 <- as.data.frame(df2, stringsAsFactors = FALSE, optional = TRUE)
  
  if (wrap_formula && "Formula" %in% colnames(df2) && ncol(df2) >= 2) {
    n <- ncol(df2)
    colspec <- .equal_width_colspec(n)
  } else {
    n <- ncol(df2)
    pre <- character(0)
    
    if (identical(colnames(df2)[1:2], c("Variable","Role"))) {
      pre <- character(0)
      colspec <- .equal_width_colspec(n)
      
    } else {
      #standardized nonroles branch
      colspec <- .equal_width_colspec(n)
    }
  }
  #sets to same width as modelsummary
  labs <- colnames(df2)
  if (identical(labs[1:2], c("Variable","Role"))) {
    # Keep first two headers left; center the grid labels INSIDE their p{…} cells.
    rest <- sprintf("{\\centering %s\\par}", .tex_escape(labs[3:length(labs)]))
    header <- paste(c(.tex_escape(labs[1]), .tex_escape(labs[2]), rest), collapse = " & ")
  } else {
    header <- paste(.tex_escape(labs), collapse = " & ")
  }
  
  body   <- apply(df2, 1L, function(r) paste(r, collapse = " & "))
  
  core <- c(
    pre,
    paste0("\\begin{longtable}{", colspec, "}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    paste0(body, " \\\\"),
    "\\bottomrule",
    "\\end{longtable}"
  )
  
  if (wrap_center) {
    c("\\begin{center}",
      "\\setlength{\\LTleft}{0pt}\\setlength{\\LTright}{0pt}",
      core,
      "\\end{center}")
  } else {
    core
  }
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

.msummary_to_longtable_centered <- function(mods, wrap_center = TRUE) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    return(c("% modelsummary not installed; skipping model comparison"))
  }
  
  df <- modelsummary::msummary(
    mods,
    output   = "data.frame",
    stars    = TRUE,
    gof_omit = "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma"
  )
  
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(c("% empty modelsummary table"))
  }
  
  # glue std.errors in parentheses and collapse onto the estimate rows
  df[] <- lapply(df, function(x) { y <- as.character(x); y[is.na(y)] <- ""; y })
  meta_cols  <- intersect(c("part","term","statistic"), names(df))
  model_cols <- setdiff(names(df), meta_cols)
  est <- df[df$part == "estimates", , drop = FALSE]
  gof <- df[df$part == "gof",       , drop = FALSE]
  
  term_order <- unique(est$term[est$statistic == "estimate"])
  rows <- list()
  for (tm in term_order) {
    e <- est[est$term == tm & est$statistic == "estimate", model_cols, drop = FALSE]
    s <- est[est$term == tm & est$statistic == "std.error", model_cols, drop = FALSE]
    if (!nrow(s)) s <- as.data.frame(as.list(rep("", length(model_cols))), stringsAsFactors = FALSE)
    names(s) <- model_cols
    s[] <- lapply(s, function(v) ifelse(v == "" | is.na(v), "", paste0(" (", v, ")")))
    rows[[length(rows) + 1L]] <- data.frame(
      Term = tm,
      as.list(mapply(paste0, e[1,], s[1,], SIMPLIFY = TRUE)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  if (nrow(gof)) {
    for (i in seq_len(nrow(gof))) {
      g <- gof[i, , drop = FALSE]
      rows[[length(rows) + 1L]] <- data.frame(
        Term = g$term,
        as.list(g[1, model_cols, drop = FALSE]),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  pretty <- do.call(rbind, rows)
  colnames(pretty) <- c("Term", model_cols)
  # Make the leftmost column robust: avoids TeX errors on leading '(' etc.
  pretty$Term <- sprintf("\\textnormal{%s}", pretty$Term)
  
  # Build longtable WITHOUT its own center wrapper (Term is raw so macros survive)
  core <- .df_to_longtable_centered(pretty, wrap_center = FALSE, raw_cols = "Term")
  body <- c("\\begingroup\\renewcommand{\\arraystretch}{1.08}", core, "\\endgroup")
  
  # NEW: bulletproof against “((…))” doubling (rare toolchain hiccup)
  single <- paste(body, collapse = "\n")
  single <- gsub("\\(\\s*\\(", "(", single)
  single <- gsub("\\)\\s*\\)", ")", single)
  strsplit(single, "\n", fixed = TRUE)[[1]]
}
################################################################################

# R/report_latex.R
## Minimal LaTeX fragment writer for DAGassist. Produces a PREAMBLE-LESS snippet.
## Include it in your paper with: \input{path/to/fragment.tex}
## Pretty LaTeX fragment: centered longtables, tidy headers, adj sets, modelsummary

.report_latex_fragment <- function(res, out) {
  stopifnot(is.character(out), length(out) == 1L)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  
  roles  <- tryCatch(res$roles_df, error = function(e) NULL)
  mods   <- tryCatch(res$models,    error = function(e) NULL)
  msets  <- tryCatch(res$min_sets,  error = function(e) list())
  canon  <- tryCatch(res$canon,     error = function(e) character(0))
  
  # Build the single Notes line with p-value legend + controls summary
  ctrl_min <- if (length(msets)) .set_brace(msets[[1]]) else "{}"
  ctrl_can <- if (length(canon)) .set_brace(canon) else "{}"
  
  notes_line <- paste0(
    "{\\footnotesize ",
    "\\emph{Stars Key:} + p $< 0.1$, * p $< 0.05$, ** p $< 0.01$, *** p $< 0.001$.\\\\",
    "\\quad \\textit{Minimal controls:} ",   ctrl_min, "\\\\",
    "\\quad \\textit{Canonical controls:} ", ctrl_can,
    "}"
  )
  
  lines <- c(
    "% ---- DAGassist LaTeX fragment (no preamble) ----",
    "% Requires: \\usepackage{longtable}, \\usepackage{booktabs}",
    "\\begingroup\\footnotesize",
    "\\setlength{\\LTleft}{0pt}\\setlength{\\LTright}{0pt}",
    "\\setlength{\\tabcolsep}{4pt}",
    "\\renewcommand{\\arraystretch}{0.95}",
    "\\setlength{\\aboverulesep}{0pt}\\setlength{\\belowrulesep}{0pt}",
    "\\setlength{\\LTpre}{0pt}\\setlength{\\LTpost}{0pt}", 
    "\\begin{center}\\textbf{DAGassist Report:}\\end{center}",
    
    "\\begin{center}",
    {
      roles <- tryCatch(res$roles_df, error = function(e) NULL)
      if (is.data.frame(roles) && nrow(roles)) {
        rp <- .roles_pretty(roles)
        c(.df_to_longtable_centered(rp, wrap_center = FALSE),
          "\\vspace{1pt}")   ## <- was +2pt; make it slightly negative to literally attach
      } else character(0)
    },
    {
      mods <- tryCatch(res$models, error = function(e) NULL)
      if (!is.null(mods)) {
        .msummary_to_longtable_centered(mods, wrap_center = FALSE)
      } else character(0)
    },
    "\\end{center}",
    
    # Notes: stars, then each controls line on its own, indented
    {
      msets <- tryCatch(res$min_sets, error = function(e) list())
      canon <- tryCatch(res$canon,    error = function(e) character(0))
      min_str   <- if (length(msets)) .set_brace(msets[[1]]) else "{}"
      canon_str <- .set_brace(canon)
      c(
        "{\\footnotesize \\emph{Notes:} + p $< 0.1$, * p $< 0.05$, ** p $< 0.01$, *** p $< 0.001$.\\\\",
        paste0("\\hspace*{1.5em}\\textit{Controls (minimal):} ", min_str, "\\\\"),
        paste0("\\hspace*{1.5em}\\textit{Controls (canonical):} ", canon_str, ".}"))
    },
    
    "\\endgroup"
  )
  
  writeLines(unlist(lines), out, useBytes = TRUE)
  invisible(out)
}