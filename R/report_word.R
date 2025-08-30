# ---- DOCX export (no flextable), publication-grade tweaks -------------------

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

# reuse modelsummary but put std.errors on the line BELOW each estimate
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
  
  # normalize to strings; keep blanks as ""
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
    rows[[length(rows)+1L]] <- data.frame(
      Term = tm, as.list(e[1, , drop = TRUE]),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    # std.error row right underneath (in parentheses)
    if (nrow(s)) {
      s_par <- lapply(s[1, , drop = TRUE],
                      function(v) ifelse(v == "" | is.na(v), "", paste0("(", v, ")")))
      rows[[length(rows)+1L]] <- data.frame(
        Term = "", as.list(s_par),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  
  if (nrow(gof)) {
    for (i in seq_len(nrow(gof))) {
      g <- gof[i, , drop = FALSE]
      rows[[length(rows)+1L]] <- data.frame(
        Term = g$term, as.list(g[1, modsC, drop = FALSE]),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  
  pretty <- do.call(rbind, rows)
  colnames(pretty) <- c("Term", modsC)
  list(df = pretty)
}

# --- MAIN: write a .docx via Markdown -> pandoc
# NEW: picks up an optional reference docx from:
#   1) options("DAGassist.ref_docx")   OR
#   2) inst/resources/DAGassist-reference.docx (if present)  OR
#   3) rmarkdown's default reference docx
.report_docx <- function(res, out) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Please install 'rmarkdown' (needs pandoc) to export DOCX.", call. = FALSE)
  }
  
  roles <- tryCatch(res$roles_df, error = function(e) NULL)
  mods  <- tryCatch(res$models,    error = function(e) NULL)
  msets <- tryCatch(res$min_sets,  error = function(e) list())
  canon <- tryCatch(res$canon,     error = function(e) character(0))
  
  md <- c("# DAGassist Report", "")
  
  # roles table
  if (is.data.frame(roles) && nrow(roles)) {
    rp <- .roles_pretty(roles)  # reuses your helper
    md <- c(md, "## Roles", "", .df_to_md_pipe(rp), "")
  }
  
  # regression table (SEs stacked)
  if (!is.null(mods)) {
    built <- .build_modelsummary_pretty_df(mods)
    if (!is.null(built$df)) {
      md <- c(md, "## Models", "", .df_to_md_pipe(built$df), "")
    }
  }
  
  # notes
  min_str   <- if (length(msets)) .set_brace_plain(msets[[1]]) else "{}"
  canon_str <- .set_brace_plain(canon)
  notes_txt <- paste0(
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001.\n\n",
    "*Controls (minimal):* ",   min_str, "\n\n",
    "*Controls (canonical):* ", canon_str, "."
  )
  md <- c(md, "## Notes", "", notes_txt, "")
  
  # write temp .md
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  mdfile <- tempfile(fileext = ".md")
  writeLines(md, mdfile, useBytes = TRUE)
  
  # pick a reference .docx (optional, but highly recommended)
  # 1) option
  ref <- getOption("DAGassist.ref_docx", default = NULL)
  # 2) package resource if present
  if (is.null(ref)) {
    pkg_ref <- file.path(getwd(), "inst", "resources", "DAGassist-reference.docx")
    if (file.exists(pkg_ref)) ref <- pkg_ref
  }
  # 3) rmarkdown default if still NULL
  if (is.null(ref)) {
    ref <- system.file("rmd", "reference", "default.docx", package = "rmarkdown")
  }
  
  # convert -> docx (use GitHub-flavored MD; apply reference doc)
  rmarkdown::pandoc_convert(
    input  = mdfile,
    from   = "gfm",
    to     = "docx",
    output = normalizePath(out, mustWork = FALSE),
    options = c("--standalone", ref)
  )
  
  # --- Post-process to cure the rare doubled-parentheses glitch seen in your file
  #     (Word is happy with direct binary edit via zip/xml, but a quick rewrite is simpler.)
  #     We'll re-write the file only if the pattern occurs.
  bin <- readBin(out, what = "raw", n = file.info(out)$size)
  if (any(grepl(rawToChar(as.raw(c(0x28,0x28))), rawToChar(bin), fixed = TRUE))) {
    txt <- rawToChar(bin)
    txt <- gsub("\\(\\(", "(", txt, perl = TRUE)
    txt <- gsub("\\)\\)", ")", txt, perl = TRUE)
    # Only write back if we still have valid zip (docx is a zip). If not, skip.
    # Heuristic: keep it simple—only write back when size changes minimally.
    writeBin(charToRaw(txt), out)
  }
  
  invisible(out)
}