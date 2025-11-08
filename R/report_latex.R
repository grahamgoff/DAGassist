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

.msummary_to_longtable_centered <- function(mods, coef_rename=NULL, coef_omit=NULL) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    return(c("% modelsummary not installed; skipping model comparison"))
  }
  
  #build a modelsummary map: names = raw coef names, values = labels
  #this is similar to just running coef_rename but it deals with escape
  cm <- NULL
  if (length(coef_rename)) {
    #manually escape to avoid issues
    esc <- function(s) gsub("([%_&#{}~^$\\\\])", "\\\\\\1", s, perl = TRUE)
    #escape is false in the modelsummary spec to allow manual \mbox 
    #we need \mbox to avoid weird auto line breaks in long labels
    nowrap <- function(s) paste0("\\mbox{", esc(s), "}")
    vals <- vapply(unname(coef_rename), nowrap, character(1))
    names(vals) <- names(coef_rename)
    cm <- vals
  }
  
  #suppress the once-per-session warning about siunitx
  out <- suppressWarnings(
    modelsummary::modelsummary(
      mods,
      output = "latex",
      stars = TRUE,
      escape = FALSE,
      gof_omit = "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma",
      booktabs = TRUE,
      coef_omit = coef_omit,    
      coef_rename=cm
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
  roles  <- tryCatch(res$roles_df, error = function(e) NULL)
  mods   <- tryCatch(res$models, error = function(e) NULL)
  msets  <- tryCatch(res$min_sets, error = function(e) list())
  canon  <- tryCatch(res$canon, error = function(e) character(0))
  show <- tryCatch(res$show, error = function(e) "all")
  
  # Build the single Notes line with p-value legend + controls summary
  ctrl_min <- if (length(msets)) .set_brace(msets[[1]]) else "{}"
  ctrl_can <- if (length(canon)) .set_brace(canon) else "{}"
  
  lines <- c(
    "% ---- DAGassist LaTeX fragment (no preamble) ----",
    "% Requires: \\usepackage{tabularray} \\UseTblrLibrary{booktabs,siunitx,talltblr}",
    "\\begingroup\\footnotesize",
    {
      if (show != "models" && is.data.frame(roles) && nrow(roles)) {
        c(.df_to_longtable_centered(.roles_pretty(roles)),
          "% no vertical glue between tables",
          "\\nointerlineskip")
      } else character(0)
    },
    {
      if (show != "roles" && !is.null(mods)) .msummary_to_longtable_centered(mods, 
                                                          coef_rename=res$coef_rename,
                                                          coef_omit = res$coef_omit) 
      else character(0)
    },
    "\\par\\endgroup",
    #adtocounter reduces the table ref number by one because there are two 
    #tabular objects in output, so the counter increments by 2 naturally.
    #only do this if printing the whole report because that is the only case
    #in which there will be two tabular objects in the report
    if (show == "all"){
      "\\addtocounter{table}{-1}"
    }else character(0),
    {
      notes <- c(
        "\\vspace{1em}",
        "\\footnotesize"
      )
      
      if (show != "roles") {
        msets <- tryCatch(res$min_sets, error = function(e) list())
        canon <- tryCatch(res$canon,    error = function(e) character(0))
        min_str   <- if (length(msets)) .set_brace(msets[[1]]) else "{}"
        canon_str <- .set_brace(canon)
        
        notes <- c(
          notes,
          paste0("\\textit{Controls (minimal):} ",   min_str,   "\\\\"),
          paste0("\\textit{Controls (canonical):} ", canon_str)
        )
        
        if (!is.null(res$unevaluated_str) && nzchar(res$unevaluated_str)) {
          notes <- c(
            notes,
            sprintf("\\textit{Unevaluated regressors (not in DAG):} {%s}",
                    .tex_escape(res$unevaluated_str))
          )
        }
      }
      
      notes  # always return a character vector, irrespective of show param
    }
  )
  
  #write to file if out supplied, else print to console
  if (is.null(out)) {
    cat(paste(unlist(lines), collapse = "\n"), "\n")
    invisible(NULL)
  } else {
    dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
    writeLines(unlist(lines), out, useBytes = TRUE)
    invisible(out)
  }
}