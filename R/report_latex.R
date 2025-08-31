############################ INTERNAL HELPERS ##################################
# latex (modelsummary) → longtblr
.to_longtblr <- function(x) {
  # strip float wrappers/centering
  x <- gsub("\\\\begin\\{table\\}(\\[[^\\]]*\\])?\\s*", "", x, perl = TRUE)
  x <- gsub("\\\\end\\{table\\}\\s*", "", x, perl = TRUE)
  x <- gsub("\\\\centering\\s*", "", x, perl = TRUE)
  
  # tblr/talltblr -> longtblr
  x <- gsub("\\\\begin\\{talltblr\\}", "\\\\begin{longtblr}", x, perl = TRUE)
  x <- gsub("\\\\end\\{talltblr\\}",   "\\\\end{longtblr}",   x, perl = TRUE)
  x <- gsub("\\\\begin\\{tblr\\}",     "\\\\begin{longtblr}", x, perl = TRUE)
  x <- gsub("\\\\end\\{tblr\\}",       "\\\\end{longtblr}",   x, perl = TRUE)
  
  # remove illegal outer keys in [ ... ]
  x <- gsub("(\\\\begin\\{longtblr\\}\\[[^\\]]*)\\b(?:width|abovesep|belowsep|presep|postsep|rowsep)\\s*=\\s*[^,\\]]+(,)?",
            "\\1\\2", x, perl = TRUE)
  x <- gsub("\\\\begin\\{longtblr\\}\\s*\\[\\s*,", "\\\\begin{longtblr}[", x, perl = TRUE)
  x <- gsub(",\\]", "]", x, perl = TRUE)
  
  # if an outer [...] already exists, inject the space throttling specs at its start
  x <- gsub("(\\\\begin\\{longtblr\\}\\[)\\s*", "\\1presep=0pt,postsep=0pt,", x, perl = TRUE)
  
  # if there is NO outer [...], create one
  x <- gsub("\\\\begin\\{longtblr\\}(?!\\[)\\s*\\{",
            "\\\\begin{longtblr}[presep=0pt,postsep=0pt]{",
            x, perl = TRUE)
  
  # ensure INNER { ... } has the same keys as the first table
  x <- gsub("\\\\begin\\{longtblr\\}(\\[[^\\]]*\\])?\\s*\\{",
            "\\\\begin{longtblr}\\1{width=\\\\textwidth,presep=0pt,postsep=0pt,abovesep=0pt,belowsep=0pt,rowsep=0pt,",
            x, perl = TRUE)
  
  # flexible columns
  x <- gsub("Q\\[", "X[", x, perl = TRUE)
  
  # optional: align the look by removing the second table's top rule
  x <- sub("\\\\toprule", "", x, perl = TRUE)
  
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


.df_to_longtable_centered <- function(df, raw_cols = character()) {
  stopifnot(is.data.frame(df))
  df2 <- df
  
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
    df2[[nm]] <- if (nm %in% raw_cols) ifelse(is.na(df2[[nm]]), "", as.character(df2[[nm]])) else esc(df2[[nm]])
  }
  
  n <- ncol(df2)
  # first column left, the rest centered; all flexible X to fill \textwidth
  colspec <- paste(c("X[l]", rep("X[c]", max(0, n - 1))), collapse = "")
  
  header  <- paste(colnames(df2), collapse = " & ")
  rows    <- apply(df2, 1L, function(r) paste(r, collapse = " & "))
  
  c(
    "\\begin{longtblr}[presep=0pt, postsep=0pt, caption={DAGassist Report:}, label={tab:dagassist}]%",
    sprintf("{width=\\textwidth,colspec={%s}}", colspec),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    paste0(rows, " \\\\"),
    "\\bottomrule",
    "\\end{longtblr}"
  )
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
  
  return(strsplit(out, "\n")[[1]])
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
  
  lines <- c(
    "% ---- DAGassist LaTeX fragment (no preamble) ----",
    "% Requires: \\usepackage{tabularray} \\UseTblrLibrary{booktabs}",
    "\\begingroup\\footnotesize",
    #"\\setlength{\\tabcolsep}{4pt}",
    #"\\renewcommand{\\arraystretch}{0.95}",
    #"\\setlength{\\aboverulesep}{0ex}\\setlength{\\belowrulesep}{0ex}",

    # in .report_latex_fragment (unchanged pieces omitted)
    #"\\begingroup\\setlength{\\parskip}{0pt}\\setlength{\\topsep}{0pt}\\setlength{\\partopsep}{0pt}\\centering",
    {
      if (is.data.frame(roles) && nrow(roles)) {
        c(.df_to_longtable_centered(.roles_pretty(roles)),
          "% no vertical glue between tables",
          "\\nointerlineskip")
      } else character(0)
    },
    {
      if (!is.null(mods)) .msummary_to_longtable_centered(mods) else character(0)
    },
    "\\par\\endgroup",
    
    # Notes: stars, then each controls line on its own, indented
    {
      msets <- tryCatch(res$min_sets, error = function(e) list())
      canon <- tryCatch(res$canon,    error = function(e) character(0))
      min_str   <- if (length(msets)) .set_brace(msets[[1]]) else "{}"
      canon_str <- .set_brace(canon)
      c("\\vspace{1em}", 
        "\\footnotesize",
        paste0("\\hspace*{1em}\\textit{Controls (minimal):} ", min_str, "\\\\"),
        paste0("\\hspace*{2.5em}\\textit{Controls (canonical):} ", canon_str))
    }
  )
  
  writeLines(unlist(lines), out, useBytes = TRUE)
  invisible(out)
}