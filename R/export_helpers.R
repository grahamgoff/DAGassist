# R/export_helpers.R  (internal helpers, not exported)

.MS_GOF_OMIT <- "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma"

.set_brace_plain <- function(s) {
  s <- as.character(s); if (!length(s)) return("{}")
  paste0("{", paste(s, collapse = ", "), "}")
}

.df_to_md_pipe <- function(df) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    align <- rep("c", ncol(df)); align[1] <- "l"
    return(as.character(knitr::kable(df, format = "pipe", align = align)))
  }
  esc <- function(x) gsub("\\|", "\\\\|", x, perl = TRUE)
  hdr <- paste(names(df), collapse = " | ")
  sep <- paste(ifelse(seq_len(ncol(df)) == 1, ":---", ":---:"), collapse = " | ")
  rows <- apply(df, 1L, function(r) paste(esc(r), collapse = " | "))
  paste0("| ", hdr, " |\n| ", sep, " |\n", paste0("| ", rows, " |", collapse = "\n"))
}

.msummary_to_markdown <- function(mods) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    return(c("% modelsummary not installed; skipping model comparison"))
  }
  out <- modelsummary::modelsummary(
    mods, output = "markdown", stars = TRUE, escape = FALSE, gof_omit = .MS_GOF_OMIT
  )
  out <- paste(as.character(out), collapse = "\n")
  strsplit(out, "\n")[[1]]
}

.find_refdoc <- function() {
  opt <- getOption("DAGassist.ref_docx", "")
  if (nzchar(opt) && file.exists(opt)) return(normalizePath(opt, winslash = "/"))
  pkg <- system.file("resources", "DAGassist-reference.docx", package = "DAGassist")
  if (nzchar(pkg) && file.exists(pkg)) return(normalizePath(pkg, winslash = "/"))
  pd  <- system.file("rmd", "reference", "default.docx", package = "rmarkdown")
  if (nzchar(pd) && file.exists(pd)) return(normalizePath(pd, winslash = "/"))
  ""
}

.pandoc_docx <- function(md_lines, out, refdoc = .find_refdoc()) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Please install 'rmarkdown' (needs pandoc) to export DOCX.", call. = FALSE)
  }
  if (!rmarkdown::pandoc_available("2.0")) {
    stop("Pandoc 2+ not found. Install pandoc or run from RStudio which bundles it.", call. = FALSE)
  }
  
  # write temp markdown
  mdfile <- tempfile(fileext = ".md")
  writeLines(md_lines, mdfile, useBytes = TRUE)
  
  # resolve output location & ensure directory
  resolved_out <- normalizePath(out, winslash = "/", mustWork = FALSE)
  out_dir  <- dirname(resolved_out)
  out_file <- basename(resolved_out)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # options
  opts <- c("--standalone")
  if (nzchar(refdoc) && file.exists(refdoc)) {
    opts <- c(opts, paste0("--reference-doc=", normalizePath(refdoc, winslash = "/", mustWork = FALSE)))
  }
  
  # convert with wd pinned to output dir
  rmarkdown::pandoc_convert(
    input  = mdfile,
    from   = "markdown_strict+pipe_tables+grid_tables+raw_html",
    to     = "docx",
    output = out_file,
    options = opts,
    wd     = out_dir
  )
  
  # verify creation
  if (!file.exists(resolved_out)) {
    ver <- tryCatch(rmarkdown::pandoc_version(), error = function(e) NA)
    stop("DOCX export reported success but no file at: ", resolved_out,
         "\nPandoc version: ", as.character(ver), call. = FALSE)
  }
  invisible(resolved_out)
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

.build_modelsummary_pretty_df <- function(mods) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    return(list(df = NULL, gof_first = NA_character_))
  }
  df <- modelsummary::msummary(mods, output = "data.frame",
                               stars = TRUE, gof_omit = .MS_GOF_OMIT)
  if (!is.data.frame(df) || !nrow(df)) {
    return(list(df = NULL, gof_first = NA_character_))
  }
  
  df[]  <- lapply(df, function(x){y <- as.character(x); y[is.na(y)] <- ""; y})
  meta  <- intersect(c("part","term","statistic"), names(df))
  modsC <- setdiff(names(df), meta)
  est   <- df[df$part == "estimates", , drop = FALSE]
  gof   <- df[df$part == "gof",       , drop = FALSE]
  
  rows <- list()
  for (tm in unique(est$term[est$statistic == "estimate"])) {
    e <- est[est$term == tm & est$statistic == "estimate",  modsC, drop = FALSE]
    s <- est[est$term == tm & est$statistic == "std.error", modsC, drop = FALSE]
    rows[[length(rows)+1L]] <- data.frame(Term = tm, as.list(e[1, , drop = TRUE]),
                                          check.names = FALSE, stringsAsFactors = FALSE)
    if (nrow(s)) {
      s_par <- lapply(s[1, , drop = TRUE], function(v) {
        v <- ifelse(is.na(v) | v == "", "", v)
        if (!nzchar(v)) v else if (grepl("^\\s*\\(.+\\)\\s*$", v)) v else paste0("(", v, ")")
      })
      rows[[length(rows)+1L]] <- data.frame(Term = "\u00A0", as.list(s_par),
                                            check.names = FALSE, stringsAsFactors = FALSE)
    }
  }
  if (nrow(gof)) for (i in seq_len(nrow(gof))) {
    g <- gof[i, , drop = FALSE]
    rows[[length(rows)+1L]] <- data.frame(Term = g$term, as.list(g[1, modsC, drop = FALSE]),
                                          check.names = FALSE, stringsAsFactors = FALSE)
  }
  
  # strict column normalization (avoids match.names issues)
  cols <- c("Term", modsC)
  rows <- lapply(rows, function(d){
    miss <- setdiff(cols, names(d)); if (length(miss)) d[miss] <- ""
    d <- d[, cols, drop = FALSE]; d[] <- lapply(d, as.character); d
  })
  pretty <- do.call(rbind, rows)
  rownames(pretty) <- NULL; colnames(pretty) <- cols
  
  list(df = pretty, gof_first = if (nrow(gof)) gof$term[1] else NA_character_)
}