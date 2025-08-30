.find_refdoc <- function() {
  # 1) user override via option
  opt <- getOption("DAGassist.ref_docx", "")
  if (nzchar(opt) && file.exists(opt)) return(normalizePath(opt))
  
  # 2) package resource shipped with DAGassist
  pkg <- system.file("resources", "DAGassist-reference.docx", package = "DAGassist")
  if (nzchar(pkg) && file.exists(pkg)) return(normalizePath(pkg))
  
  # 3) rmarkdown's bundled default reference docx
  pd <- system.file("rmd", "reference", "default.docx", package = "rmarkdown")
  if (nzchar(pd) && file.exists(pd)) return(normalizePath(pd))
  
  # 4) fallback: let pandoc use its internal defaults
  ""
}

# simple braces for Notes in Word/Markdown
.set_brace_plain <- function(s) {
  s <- as.character(s); if (!length(s)) return("{}")
  paste0("{", paste(s, collapse = ", "), "}")
}

# turn a data.frame into a Markdown pipe table (no extra deps if knitr absent)
.df_to_md_pipe <- function(df) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    align <- rep("c", ncol(df)); align[1] <- "l"      # left Terms, center others
    return(as.character(knitr::kable(df, format = "pipe", align = align)))
  }
  esc <- function(x) gsub("\\|", "\\\\|", x, perl = TRUE)
  hdr <- paste(names(df), collapse = " | ")
  sep <- paste(ifelse(seq_len(ncol(df)) == 1, ":---", ":---:"), collapse = " | ")
  rows <- apply(df, 1L, function(r) paste(esc(r), collapse = " | "))
  paste0("| ", hdr, " |\n| ", sep, " |\n", paste0("| ", rows, " |", collapse = "\n"))
}

# --- ONLY wrap SEs if not already in (...) and keep the SE row stable in Word
.build_modelsummary_pretty_df <- function(mods) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    stop("Please install 'modelsummary' to export regression tables.", call. = FALSE)
  }
  if (!rmarkdown::pandoc_available("2.0")) {
    stop("Pandoc 2+ not found. Install pandoc or run from RStudio which bundles it.", call. = FALSE)
  }
  
  df <- modelsummary::msummary(
    mods, output = "data.frame", stars = TRUE,
    gof_omit = "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma"
  )
  if (!is.data.frame(df) || nrow(df) == 0L) return(list(df = NULL))
  
  df[] <- lapply(df, function(x) { y <- as.character(x); y[is.na(y)] <- ""; y })
  meta  <- intersect(c("part","term","statistic"), names(df))
  modsC <- setdiff(names(df), meta)
  est   <- df[df$part == "estimates", , drop = FALSE]
  gof   <- df[df$part == "gof",       , drop = FALSE]
  
  term_order <- unique(est$term[est$statistic == "estimate"])
  rows <- list()
  
  for (tm in term_order) {
    e <- est[est$term == tm & est$statistic == "estimate",  modsC, drop = FALSE]
    s <- est[est$term == tm & est$statistic == "std.error", modsC, drop = FALSE]
    
    # estimates row
    rows[[length(rows) + 1L]] <- data.frame(
      Term = tm, as.list(e[1, , drop = TRUE]),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    
    # std.error row right underneath (only add () if not already wrapped)
    if (nrow(s)) {
      s_par <- lapply(s[1, , drop = TRUE], function(v) {
        v <- ifelse(is.na(v) | v == "", "", v)
        if (!nzchar(v)) return(v)
        if (grepl("^\\s*\\(.+\\)\\s*$", v)) v else paste0("(", v, ")")
      })
      rows[[length(rows) + 1L]] <- data.frame(
        Term = "\u00A0",  # non-breaking space so Word keeps the row tidy
        as.list(s_par),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  
  if (nrow(gof)) {
    for (i in seq_len(nrow(gof))) {
      g <- gof[i, , drop = FALSE]
      rows[[length(rows) + 1L]] <- data.frame(
        Term = g$term, as.list(g[1, modsC, drop = FALSE]),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  
  pretty <- do.call(rbind, rows)
  colnames(pretty) <- c("Term", modsC)
  list(df = pretty)
}

# --- DOCX export: safe refdoc lookup + stricter markdown reader
.report_docx <- function(res, out) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Please install 'rmarkdown' (needs pandoc) to export DOCX.", call. = FALSE)
  }
  
  roles <- tryCatch(res$roles_df, error = function(e) NULL)
  mods  <- tryCatch(res$models,    error = function(e) NULL)
  msets <- tryCatch(res$min_sets,  error = function(e) list())
  canon <- tryCatch(res$canon,     error = function(e) character(0))
  
  # --- tiny MD helpers already defined in your file: .roles_pretty, .df_to_md_pipe, .set_brace_plain
  
  md <- c("# DAGassist Report", "")
  
  if (is.data.frame(roles) && nrow(roles)) {
    rp <- .roles_pretty(roles)
    md <- c(md, "## Roles", "", .df_to_md_pipe(rp), "")
  }
  
  if (!is.null(mods)) {
    built <- .build_modelsummary_pretty_df(mods)
    if (!is.null(built$df)) {
      md <- c(md, "## Models", "", .df_to_md_pipe(built$df), "")
    }
  }
  
  min_str   <- if (length(msets)) .set_brace_plain(msets[[1]]) else "{}"
  canon_str <- .set_brace_plain(canon)
  notes_txt <- paste0(
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001.\n\n",
    "*Controls (minimal):* ",   min_str, "\n\n",
    "*Controls (canonical):* ", canon_str, "."
  )
  md <- c(md, "## Notes", "", notes_txt, "")
  
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  mdfile <- tempfile(fileext = ".md")
  writeLines(md, mdfile, useBytes = TRUE)
  
  # reference doc: option -> package resource -> rmarkdown default -> none
  ref <- getOption("DAGassist.ref_docx", "")
  if (!nzchar(ref) || !file.exists(ref)) {
    pkg_ref <- system.file("resources", "DAGassist-reference.docx", package = "DAGassist")
    if (nzchar(pkg_ref) && file.exists(pkg_ref)) ref <- pkg_ref
  }
  if (!nzchar(ref) || !file.exists(ref)) {
    ref <- system.file("rmd", "reference", "default.docx", package = "rmarkdown")
    if (!nzchar(ref) || !file.exists(ref)) ref <- ""
  }
  
  opts <- c("--standalone")
  if (nzchar(ref)) opts <- c(opts, paste0("--reference-doc=", normalizePath(ref)))
  
  rmarkdown::pandoc_convert(
    input  = mdfile,
    from   = "markdown_strict+pipe_tables+raw_html",
    to     = "docx",
    output = normalizePath(out, mustWork = FALSE),
    options = opts
  )
  
  invisible(out)
}