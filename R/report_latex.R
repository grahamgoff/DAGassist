############################ INTERNAL HELPERS ##################################
# talltblr to longtblr 
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
  ##weight columns
  k <- max(0L, n - 2L)# number of flag columns
  
  role_w <- 15L # fixed share for Role
  flag_w <- 8L # fixed share for each flag column
  
  # map longest variable label length and cap at 70
  plain1 <- if (n >= 1) gsub("\\\\[a-z]+\\{[^}]*\\}", "", df2[[1]]) else character()
  L<- if (length(plain1)) max(nchar(plain1), na.rm = TRUE) else 0L
  
  # piecewise var col width
  var_w <- if (L <= 15) 35L # short labels
  else if (L <= 22) 50L 
  else if (L <= 30) 60L # longer phrases
  else 70L # absolute cap 
  
  # build column width spec
  weights <- c(var_w, role_w, rep(flag_w, k))
  aligns  <- c("l", "l", rep("c", k))
  colspec <- paste0(mapply(function(w, a) sprintf("X[%d,%s]", w, a), weights, aligns), collapse = "")
  
  header <- paste(colnames(df2), collapse = " & ")
  rows   <- apply(df2, 1L, function(r) paste(r, collapse = " & "))
  
  c(# tighten spacing + give TeX a little elasticity to avoid overfull boxes
    "\\begingroup\\setlength{\\emergencystretch}{3em}",
    # small colsep so center columns "touch" visually
    "\\begin{longtblr}[presep=0pt, postsep=0pt, caption={DAGassist Report:}, label={tab:dagassist}]%",
    sprintf("{width=\\textwidth,colsep=1.5pt,rowsep=0pt,abovesep=0pt,belowsep=0pt,colspec={%s}}", colspec),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    paste0(rows, " \\\\"),
    "\\bottomrule",
    "\\end{longtblr}",
    "\\endgroup"
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
  
  #suppress the once-per-session warning about siunitx
  out <- suppressWarnings(
    modelsummary::modelsummary(
      mods,
      output = "latex",
      stars = TRUE,
      escape = TRUE,
      gof_omit = "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma",
      booktabs = TRUE
    )
  )
  # Coerce to single string
  out <- paste(as.character(out), collapse = "\n")
  out <- .to_longtblr(out)
  strsplit(out, "\n")[[1]]
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
  mods   <- tryCatch(res$models, error = function(e) NULL)
  msets  <- tryCatch(res$min_sets, error = function(e) list())
  canon  <- tryCatch(res$canon, error = function(e) character(0))

  # Build the single Notes line with p-value legend + controls summary
  ctrl_min <- if (length(msets)) .set_brace(msets[[1]]) else "{}"
  ctrl_can <- if (length(canon)) .set_brace(canon) else "{}"
  
  lines <- c(
    "% ---- DAGassist LaTeX fragment (no preamble) ----",
    "% Requires: \\usepackage{tabularray} \\UseTblrLibrary{booktabs,siunitx,talltblr}",
    "\\begingroup\\footnotesize",
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
        paste0("\\textit{Controls (minimal):} ", min_str, "\\\\"),
        paste0("\\textit{Controls (canonical):} ", canon_str))
    }
  )
  
  writeLines(unlist(lines), out, useBytes = TRUE)
  invisible(out)
}