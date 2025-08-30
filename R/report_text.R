# ---- TXT export (ASCII, LaTeX-like layout) -----------------------------------

# tiny padder for left/center/right
.pad_align <- function(x, w, a = "l") {
  x <- ifelse(is.na(x), "", as.character(x))
  n <- nchar(x, type = "width", allowNA = FALSE, keepNA = FALSE)
  pad <- pmax(0, w - n)
  switch(a,
         "l" = paste0(x, strrep(" ", pad)),
         "r" = paste0(strrep(" ", pad), x),
         # center (bias left on odd)
         paste0(strrep(" ", floor(pad/2)), x, strrep(" ", ceiling(pad/2)))
  )
}

# build an ASCII table from a data.frame
# - align: vector of "l","c","r" (length = ncol(df))
# - header_rule: char for header rule (e.g., "=")
# - body_rule:   char for normal rules (e.g., "-")
# - midrule_after: integer vector of body row indices after which to draw a heavy rule
.ascii_table <- function(df, align, header_rule = "=", body_rule = "-",
                         midrule_after = integer(0)) {
  stopifnot(ncol(df) == length(align))
  # normalize strings
  df[] <- lapply(df, function(v) ifelse(is.na(v), "", as.character(v)))
  headers <- names(df)
  
  # compute col widths
  widths <- pmax(nchar(headers, type = "width"),
                 sapply(df, function(col) max(nchar(col, type = "width"), 0L)))
  # row helpers
  hline <- function(ch) {
    paste0("+", paste0(strrep(ch, widths + 2L), collapse = "+"), "+")
  }
  row_txt <- function(vals) {
    cells <- mapply(.pad_align, vals, widths, align, SIMPLIFY = FALSE)
    paste0("| ", paste0(unlist(cells), collapse = " | "), " |")
  }
  
  out <- character(0)
  # top + header + header rule
  out <- c(out, hline(body_rule))
  out <- c(out, row_txt(headers))
  out <- c(out, hline(header_rule))
  # body with optional heavy midrule(s)
  for (i in seq_len(nrow(df))) {
    out <- c(out, row_txt(df[i, , drop = TRUE]))
    if (length(midrule_after) && i %in% midrule_after) {
      out <- c(out, hline(header_rule))
    }
  }
  # bottom
  out <- c(out, hline(body_rule))
  out
}

# reuse modelsummary but stack std.errors on the line BELOW each estimate,
# and return the first GOF term so we can insert a midrule before GOF.
.build_modelsummary_pretty_df_txt <- function(mods) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    stop("Please install 'modelsummary' to export regression tables.", call. = FALSE)
  }
  df <- modelsummary::msummary(
    mods, output = "data.frame", stars = TRUE,
    gof_omit = "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma"
  )
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(list(df = NULL, gof_first = NA_character_))
  }
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
    # std.error row directly underneath (in parentheses)
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
  list(df = pretty, gof_first = if (nrow(gof)) gof$term[1] else NA_character_)
}

# main: write a .txt that mirrors your LaTeX fragment feel
.report_txt <- function(res, out) {
  roles <- tryCatch(res$roles_df, error = function(e) NULL)
  mods  <- tryCatch(res$models,    error = function(e) NULL)
  msets <- tryCatch(res$min_sets,  error = function(e) list())
  canon <- tryCatch(res$canon,     error = function(e) character(0))
  
  lines <- character(0)
  lines <- c(lines, "DAGassist Report:", "")
  
  # Roles table (Variable, Role, then centered grid)
  if (is.data.frame(roles) && nrow(roles)) {
    rp <- .roles_pretty(roles)  # uses your helper
    align <- c("l","l", rep("c", max(0, ncol(rp) - 2L)))
    lines <- c(lines, .ascii_table(rp, align = align,
                                   header_rule = "=", body_rule = "-"))
    lines <- c(lines, "")
  }
  
  # Models table with stacked SEs and a heavy midrule before GOF rows
  if (!is.null(mods)) {
    built <- .build_modelsummary_pretty_df_txt(mods)
    if (!is.null(built$df)) {
      tbl <- built$df
      # alignment: left term, right-align numeric-ish columns
      align <- c("l", rep("r", ncol(tbl) - 1L))
      # where to place the heavy midrule? after the last estimate/SE row
      mid_after <- integer(0)
      if (!is.na(built$gof_first)) {
        # find first GOF row in the stacked df
        gof_start <- which(tbl$Term == built$gof_first)
        if (length(gof_start)) mid_after <- gof_start - 1L
      }
      lines <- c(lines,
                 .ascii_table(tbl, align = align,
                              header_rule = "=", body_rule = "-",
                              midrule_after = mid_after),
                 "")
    }
  }
  
  # Notes (match LaTeX content)
  min_str   <- if (length(msets)) paste0("{", paste(msets[[1]], collapse = ", "), "}") else "{}"
  canon_str <- paste0("{", paste(canon,       collapse = ", "), "}")
  notes <- c(
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001.",
    paste0("  Controls (minimal):   ", min_str),
    paste0("  Controls (canonical): ", canon_str, ".")
  )
  lines <- c(lines, notes, "")
  
  # write file
  if (is.null(out) || !nzchar(out)) stop("type='text' requires `out=` file path.", call. = FALSE)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, out, useBytes = TRUE)
  invisible(out)
}