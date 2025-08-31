############################ INTERNAL HELPERS ##################################
#force same width between df and modelsummary, to make them look like a single 
#block
.equal_width_colspec <- function(n) {
  # total column count n; keep default \tabcolsep between columns
  w <- sprintf("p{\\dimexpr(\\textwidth - %d\\tabcolsep)/%d\\relax}", 2L*(n-1L), n)
  # no @{} between columns; only trim outer padding so left/right edges align
  paste0("@{}", paste(rep(w, n), collapse = ""), "@{}")
}

# Weighted colspec for the roles grid: wide left pair, tight X-grid on right
# ensures that long variable names have room and x grid is tight enough to easily
# interpret
.roles_colspec <- function(n, x_label = "Desc(Y)", var_frac = 0.66, pad_tabcolsep = 2L) {
  stopifnot(n >= 3L)
  k <- n - 2L  # number of X-grid columns
  
  pre <- c(
    sprintf("\\newlength{\\DAWxcol}\\settowidth{\\DAWxcol}{%s}", x_label),
    sprintf("\\addtolength{\\DAWxcol}{%d\\tabcolsep}", as.integer(pad_tabcolsep)),
    sprintf("\\newlength{\\DAWrem}\\setlength{\\DAWrem}{\\dimexpr\\textwidth - %d\\tabcolsep - %d\\DAWxcol\\relax}",
            2L*(n-1L), k)
  )
  
  colspec <- paste0(
    "@{}",
    sprintf("p{%.3f\\DAWrem}", var_frac),          # Variable
    sprintf("p{%.3f\\DAWrem}", 1 - var_frac),      # Role
    paste(rep("p{\\DAWxcol}", k), collapse = ""),   # X-grid (X, Y, Conf., Med., Col., Desc(Y), Desc(X), Canon)
    "@{}"
  )
  
  list(pre = pre, colspec = colspec)
}

# Minimal: keep tabularray, just make it long and non-floating.
.to_longtblr <- function(x) {
  # optional: remove outer floats and \centering
  x <- gsub("\\\\begin\\{table\\}(\\[[^\\]]*\\])?\\s*", "", x, perl = TRUE)
  x <- gsub("\\\\end\\{table\\}\\s*", "", x, perl = TRUE)
  x <- gsub("\\\\centering\\s*", "", x, perl = TRUE)
  
  # rename tblr/talltblr → longtblr (preserves options and setup blocks)
  x <- gsub("\\\\begin\\{talltblr\\}", "\\\\begin{longtblr}", x, perl = TRUE)
  x <- gsub("\\\\end\\{talltblr\\}",   "\\\\end{longtblr}",   x, perl = TRUE)
  x <- gsub("\\\\begin\\{tblr\\}",     "\\\\begin{longtblr}", x, perl = TRUE)
  x <- gsub("\\\\end\\{tblr\\}",       "\\\\end{longtblr}",   x, perl = TRUE)
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
    is_confounder = "CON",
    is_mediator   = "MED",
    is_collider   = "COL",
    is_descendant_of_outcome = "DY",
    is_descendant_of_exposure = "DX",
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


.df_to_longtable_centered <- function(df, wrap_center = TRUE, raw_cols = character()) {
  stopifnot(is.data.frame(df))
  df2 <- df
  
  # Escape everything unless listed in raw_cols
  esc <- function(x) {
    x <- as.character(x)
    x <- ifelse(is.na(x), "", x)
    x <- gsub("\\\\", "\\\\textbackslash{}", x, perl = TRUE)
    x <- gsub("([\\{\\}\\$\\#\\%\\_\\&])", "\\\\\\1", x, perl = TRUE)
    x <- gsub("~", "\\\\textasciitilde{}", x, perl = TRUE)
    x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
    x
  }
  for (nm in names(df2)) {
    if (!(nm %in% raw_cols)) df2[[nm]] <- esc(df2[[nm]]) else {
      df2[[nm]] <- ifelse(is.na(df2[[nm]]), "", as.character(df2[[nm]]))
    }
  }
  
  n <- ncol(df2)
  colspec <- paste(rep("Q[]", n), collapse = "")
  header  <- paste(esc(colnames(df2)), collapse = " & ")
  rows    <- apply(df2, 1L, function(r) paste(r, collapse = " & "))
  
  lines <- c(
    if (wrap_center) "\\begin{center}" else NULL,
    paste0("\\begin{longtblr}{colspec={", colspec, "}}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    paste0(rows, " \\\\"),
    "\\bottomrule",
    "\\end{longtblr}",
    if (wrap_center) "\\end{center}" else NULL
  )
  lines
}

.set_brace <- function(s) {
  s <- as.character(s)
  if (!length(s)) return("{}")
  paste0("{", paste(.tex_escape(s), collapse = ", "), "}")
}

.msummary_to_longtable_centered <- function(mods) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    return(c("% modelsummary not installed; skipping model comparison"))
  }
  out <- modelsummary(
    mods,
    output    = "latex",
    stars     = TRUE,
    escape    = FALSE,
    gof_omit  = "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma",
    booktabs  = TRUE)
  
  # Coerce to single string
  out <- paste(as.character(out), collapse = "\n")
  
  out <- .to_longtblr(out)
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
    "\\setlength{\\aboverulesep}{0.6ex}\\setlength{\\belowrulesep}{1.0ex}",
    "\\setlength{\\LTpre}{0pt}\\setlength{\\LTpost}{0pt}", 
    "\\begin{center}\\textbf{DAGassist Report:}\\end{center}",
    
    #this is like begin center but prevents paragraph space between the tables and notes
    "\\begingroup\\setlength{\\parskip}{0pt}\\setlength{\\topsep}{0pt}\\setlength{\\partopsep}{0pt}\\centering",
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
        .msummary_to_longtable_centered(mods)
      } else character(0)
    },
    #end center
    "\\par\\endgroup",
    
    # Notes: stars, then each controls line on its own, indented
    {
      msets <- tryCatch(res$min_sets, error = function(e) list())
      canon <- tryCatch(res$canon,    error = function(e) character(0))
      min_str   <- if (length(msets)) .set_brace(msets[[1]]) else "{}"
      canon_str <- .set_brace(canon)
      c(
        "{\\footnotesize + p $< 0.1$, * p $< 0.05$, ** p $< 0.01$, *** p $< 0.001$.\\\\",
        paste0("\\hspace*{1.5em}\\textit{Controls (minimal):} ", min_str, "\\\\"),
        paste0("\\hspace*{1.5em}\\textit{Controls (canonical):} ", canon_str, ".}"))
    },
    
    "\\endgroup"
  )
  
  writeLines(unlist(lines), out, useBytes = TRUE)
  invisible(out)
}