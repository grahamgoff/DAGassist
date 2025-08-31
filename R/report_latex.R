############################ INTERNAL HELPERS ##################################
# Single, robust colspec for the roles grid:
# - grid gets a stable share that grows mildly with #grid columns
# - Variable gets a low minimum but grows with name length, clamped
.roles_colspec_auto <- function(n, var_names,
                                var_min = 0.28, var_max = 0.60,
                                var_slope = 0.012,    # growth per char beyond 10
                                grid_base = 0.50,     # baseline grid share
                                grid_step = 0.015,    # extra per grid col beyond 7
                                grid_cap  = 0.60) {   # never exceed this
  stopifnot(n >= 3L)
  k <- n - 2L
  pre <- c(sprintf("\\newlength{\\DAWtot}\\setlength{\\DAWtot}{\\dimexpr\\textwidth - %d\\tabcolsep\\relax}", 2L*(n-1L)))
  
  grid_frac <- min(grid_cap, grid_base + grid_step * max(0, k - 7))
  
  # SAFE: compute on the actual names vector
  max_var_chars <- suppressWarnings(max(nchar(var_names), na.rm = TRUE))
  if (!is.finite(max_var_chars)) max_var_chars <- 0
  
  var_frac <- min(var_max, max(var_min, var_min + var_slope * max(0, max_var_chars - 10)))
  
  left_tot <- 1 - grid_frac
  grid_w   <- rep(grid_frac / k, k)
  
  colspec <- paste0(
    "@{}",
    sprintf("p{%.5f\\DAWtot}", left_tot * var_frac),
    sprintf("p{%.5f\\DAWtot}", left_tot * (1 - var_frac)),
    paste(sprintf("p{%.5f\\DAWtot}", grid_w), collapse = ""),
    "@{}"
  )
  list(pre = pre, colspec = colspec)
}

#force same width between df and modelsummary, to make them look like a single 
#block
.equal_width_colspec <- function(n) {
  # total column count n; keep default \tabcolsep between columns
  w <- sprintf("p{\\dimexpr(\\textwidth - %d\\tabcolsep)/%d\\relax}", 2L*(n-1L), n)
  # no @{} between columns; only trim outer padding so left/right edges align
  paste0("@{}", paste(rep(w, n), collapse = ""), "@{}")
}

# Fractional colspec for the roles grid.
# - grid_frac = fraction of total width used by ALL X-grid cols (right side)
# - var_frac  = share of the LEFT (non-grid) width given to Variable vs Role
.roles_colspec_frac <- function(n, grid_frac = 0.34, var_frac = 0.64) {
  stopifnot(n >= 3L)
  k <- n - 2L  # number of X-grid columns
  pre <- c(
    sprintf("\\newlength{\\DAWtot}\\setlength{\\DAWtot}{\\dimexpr\\textwidth - %d\\tabcolsep\\relax}", 2L*(n-1L))
  )
  colspec <- paste0(
    "@{}",
    sprintf("p{%.3f\\DAWtot}", (1 - grid_frac) * var_frac),           # Variable
    sprintf("p{%.3f\\DAWtot}", (1 - grid_frac) * (1 - var_frac)),     # Role
    paste(rep(sprintf("p{%.5f\\DAWtot}", grid_frac / (n - 2L)), k), collapse = ""),
    "@{}"
  )
  list(pre = pre, colspec = colspec)
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
      # Single, stable roles layout
      rc <- .roles_colspec_auto(n, var_names = df2[["Variable"]])
      pre     <- rc$pre
      colspec <- rc$colspec
      
      # Center the X-grid body cells so 'x' marks align
      if (n > 2L) {
        df2[, 3:n] <- lapply(df2[, 3:n, drop = FALSE],
                             function(v) ifelse(v == "" | is.na(v), "", sprintf("\\makebox[\\linewidth][c]{%s}", v)))
      }
    } else {
      pre     <- character(0)
      colspec <- .equal_width_colspec(n)
    }
  }
  #sets to same width as modelsummary
  labs <- colnames(df2)
  if (identical(labs[1:2], c("Variable","Role"))) {
    # Keep first two headers left; center the grid labels INSIDE their p{…} cells.
    rest <- sprintf("{\\centering \\mbox{%s}\\par}", .tex_escape(labs[3:length(labs)]))
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
  out <- as.character(out)              # coerce away knit_asis/other classes
  out <- paste(out, collapse = "\n")    # ensure single string
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