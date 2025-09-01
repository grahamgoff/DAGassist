# R/export_helpers.R  (internal helpers, not exported)
# GOF rows to omit
.MS_GOF_OMIT <- "IC|Log|Adj|Pseudo|AIC|BIC|F$|RMSE$|Within|Between|Std|sigma"

#return a simple, human-readable set string like "{A, B, C}".
# - input vector object "s" (characters, numbers, factors, etc.)
#     -`NA` values are converted to string "NA".
#     - whitespace in elements is preserved --- no trim
# - examples:
#     .set_brace_plain(c("A","B")) : "{A, B}"
#     .set_brace_plain(character(0)) : "{}"
#     .set_brace_plain(1:3): "{1, 2, 3}"
.set_brace_plain <- function(s) {
  #coerce to char for easy join
  s <- as.character(s)
  #return empty set for empty or null input
  if (!length(s)) return("{}")
  # join with ", " and wrap with braces
  paste0("{", paste(s, collapse = ", "), "}")
}

#convert dataframe to markdown pipe table
# - if knitr is available delegate to knitr::kable() for pretty output
# - otherwise, build plain table by hand
# regardless, first column l-aligned, else c-align
# all `|` escapred for correct parsing
.df_to_md_pipe <- function(df) {
  #make Role cells non-hyphenating in DOCX via a custom character style.
  #only affects the DOCX path, since this function is used for Word export
  if ("Role" %in% names(df)) {
    wrap_role <- function(x) {
      v <- as.character(x); v[is.na(v)] <- ""
      # Pandoc bracketed span with a DOCX custom style
      sprintf("[%s]{custom-style=\"DA_NoHyphen\"}", v)
    }
    df[["Role"]] <- wrap_role(df[["Role"]])
  }
  # use knitr::kable() when available
  if (requireNamespace("knitr", quietly = TRUE)) {
    #alignment vector: "l" for first column, else "c"
    align <- rep("c", ncol(df)); align[1] <- "l"
    #make short headers unbreakable in DOCX via inline code spans
    tokens <- c("X","Y","CON","MED","COL","DY","DX")
    nn <- names(df)
    nn[nn %in% tokens] <- sprintf("`%s`", nn[nn %in% tokens])
    
    old_names <- names(df)
    on.exit(names(df) <- old_names, add = TRUE)
    names(df) <- nn
    # kable returns char vector. coerce to plan character just to be safe
    return(as.character(knitr::kable(df, format = "pipe", align = align)))
  }
  ## fallback path: build manually
  #escape | for easy cell splitting
  esc <- function(x) gsub("\\|", "\\\\|", x, perl = TRUE)
  # unbreakable headers for the manual header path
  tokens <- c("X","Y","CON","MED","COL","DY","DX")
  nn <- names(df)
  nn[nn %in% tokens] <- sprintf("`%s`", nn[nn %in% tokens])
  
  #header row----separate columns by |
  hdr <- paste(names(df), collapse = " | ")
  # build distinct separators: 
  # :--- left align first column
  # :---: center align rest
  sep <- paste(
    ifelse(seq_len(ncol(df)) == 1, ":---", ":---:"), 
    collapse = " | "
    )
  #body rows with escape bars in each cell
  rows <- apply(df, 1L, function(r) paste(esc(r), collapse = " | "))
  
  ##stitch everything into a single markdown string
  #header, separator, and body rows
  #each pipe-row is wrapped with pipes
  paste0(
    "| ", hdr, " |\n",
    "| ", sep, " |\n",
    paste0("| ", rows, " |", collapse = "\n")
  )
}

##render list of fitted models as markdown table via modelsummary
#input: `mods` --- named list of model objects
#output: char vector markdown table
.msummary_to_markdown <- function(mods, coef_rename=NULL) {
  #if modelsummary is not installed returen a placeholder line and nudge install
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    return(c("% modelsummary not installed; skipping model comparison"))
  }
  #so it does not crash when there is no label = arg
  if (is.null(coef_rename) || (is.character(coef_rename) && !length(coef_rename))) {
    coef_rename <- NULL
  }
  #render model comparison as markdown. stars included for comparison, gof rows 
  # omitted for space 
  out <- modelsummary::modelsummary(
    mods, 
    output = "markdown", 
    stars = TRUE, 
    escape = FALSE, 
    gof_omit = .MS_GOF_OMIT,
    coef_rename = coef_rename
  )
  #normalize into string and recollapse to ensure all char no class
  out <- paste(as.character(out), collapse = "\n")
  strsplit(out, "\n")[[1]]
}

#locate ref doc to style report
##search order:
#user override via option
#template shipped with package 
#rmarkdown default
#pandoc default
#NOTE: this is scaffolding for later. there is no template.
.find_refdoc <- function() {
  #let user identify template
  opt <- getOption("DAGassist.ref_docx", "")
  #verify it exists
  if (nzchar(opt) && file.exists(opt)) return(normalizePath(opt, winslash = "/"))
  #look for bundled ref doc, which i need to make
  pkg <- system.file("resources", "DAGassist-reference.docx", package = "DAGassist")
  #this will return ""
  if (nzchar(pkg) && file.exists(pkg)) return(normalizePath(pkg, winslash = "/"))
  #fall back to rmarkdown ref doc
  pd  <- system.file("rmd", "reference", "default.docx", package = "rmarkdown")
  if (nzchar(pd) && file.exists(pd)) return(normalizePath(pd, winslash = "/"))
  #pandoc will work off template
  ""
}

###convert markdown to DOCX via pandoc
##steps:
#verify rmarkdown and pandoc are available
#write to temp .md
#create output dir and set pandoc working dir there
#call pandoc to convert markdown to docx
#confirm docx exists
.pandoc_docx <- function(md_lines, out, refdoc = .find_refdoc()) {
  #make sure we have rmarkdown 
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Please install 'rmarkdown' (needs pandoc) to export DOCX.", call. = FALSE)
  }#make sure we have pandoc
  if (!rmarkdown::pandoc_available("2.0")) {
    stop("Pandoc 2+ not found. Install pandoc or run from RStudio which bundles it.", call. = FALSE)
  }
  
  # write temp markdown
  mdfile <- tempfile(fileext = ".md")
  writeLines(md_lines, mdfile, useBytes = TRUE)
  
  # resolve output location + ensure directory
  resolved_out <- normalizePath(out, winslash = "/", mustWork = FALSE)
  out_dir  <- dirname(resolved_out)
  out_file <- basename(resolved_out)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # tell pandoc to make a full doc, not a frag
  opts <- c("--standalone")
  #pass ref doc to grab styles
  if (nzchar(refdoc) && file.exists(refdoc)) {
    opts <- c(opts, paste0("--reference-doc=", normalizePath(refdoc, winslash = "/", mustWork = FALSE)))
  }
  ##run pandoc
  # convert with wd pinned to output dir
  rmarkdown::pandoc_convert(
    input  = mdfile,
    from   = "markdown_strict+pipe_tables+grid_tables+raw_html+bracketed_spans",
    to     = "docx",
    output = out_file,
    options = opts,
    wd     = out_dir
  )
  
  # verify creation
  if (!file.exists(resolved_out)) {
    ver <- tryCatch(rmarkdown::pandoc_version(), error = function(e) NA)
    #loud fail
    stop("DOCX export reported success but no file at: ", resolved_out,
         "\nPandoc version: ", as.character(ver), call. = FALSE)
  }
  invisible(resolved_out)
}

##mkae x grid roles table. all exporters use this
#sets important vars first and sets nice lables
.roles_pretty <- function(roles) {
  #work out fo a copy to preserve orig for debug
  r <- roles
  #map column to var lab
  map <- c(
    variable = "Variable",
    role = "Role",
    is_exposure = "X",
    is_outcome = "Y",
    is_confounder = "CON",
    is_mediator = "MED",
    is_collider = "COL",
    is_descendant_of_outcome = "DY",
    is_descendant_of_exposure = "DX",
    canon = "Canon"
  )
  #set fallback labels and order
  prefer <- c("variable","role","is_exposure","is_outcome","is_confounder",
              "is_mediator","is_collider","is_descendant_of_outcome",
              "is_descendant_of_exposure","canon")
  #only keep labs that exist
  keep <- intersect(prefer, names(r))
  r <- r[, c(keep, setdiff(names(r), keep)), drop = FALSE]
  #flip bool to x for easy interp
  for (nm in names(r)) {
    if (is.logical(r[[nm]])) r[[nm]] <- ifelse(r[[nm]], "x", "")
  }
  #apply the display names that exist
  names(r) <- ifelse(names(r) %in% names(map), unname(map[names(r)]), names(r))

  r
}

##build stacked model table from objects term, SE, GOF
#output list of columns
#grab dataframe from modelsummary and reshape for excel and .txt so they dont need markdown
#normalize columns pre-rbind() to avoid name matching errors
.build_modelsummary_pretty_df <- function(mods) {
  #return empty result cleanly if no modelsummary
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    return(list(df = NULL, gof_first = NA_character_))
  }
  #grab df from modelsummary
  df <- modelsummary::msummary(
    mods, 
    output = "data.frame",
    stars = TRUE, 
    gof_omit = .MS_GOF_OMIT
    )
  #exit early if null
  if (!is.data.frame(df) || !nrow(df)) {
    return(list(df = NULL, gof_first = NA_character_))
  }
  #coerce char for easy manip and convert nas to empty string
  df[] <- lapply(df, function(x){y <- as.character(x); y[is.na(y)] <- ""; y})
  #split columns for processing
  meta <- intersect(c("part","term","statistic"), names(df))
  modsC <- setdiff(names(df), meta)
  #split est and gof rows
  est <- df[df$part == "estimates", , drop = FALSE]
  #dont mess with whitespace 
  gof  <- df[df$part == "gof", , drop = FALSE] 
  #initialize row list
  rows <- list()
  #for each coef row, make est and se rows----stack
  for (tm in unique(est$term[est$statistic == "estimate"])) {
    #pull est by row by model
    e <- est[est$term == tm & est$statistic == "estimate", modsC, drop = FALSE]
    #pull se by row by model
    s <- est[est$term == tm & est$statistic == "std.error", modsC, drop = FALSE]
    #label each est row with the actual var name, which was already standardized in .roles_pretty
    rows[[length(rows) + 1L]] <- data.frame(
      Term = tm,
      as.list(e[1, , drop = TRUE]),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    # append se row and parinth wrap
    if (nrow(s)) {
      s_par <- lapply(s[1, , drop = TRUE], function(v) {
        v <- ifelse(is.na(v) | v == "", "", v)
        if (!nzchar(v)) v else if (grepl("^\\s*\\(.+\\)\\s*$", v)) v else paste0("(", v, ")")
      })
      rows[[length(rows)+1L]] <- data.frame(
        Term = "\u00A0", #NBSP to align se rows and not repeat term
        as.list(s_par),
        check.names = FALSE, 
        stringsAsFactors = FALSE)
    }
  }
  #append gof rows to bottom in default order
  if (nrow(gof)) {
    for (i in seq_len(nrow(gof))) {
      g <- gof[i, , drop = FALSE]
      rows[[length(rows) + 1L]] <- data.frame(
        Term = g$term,
        as.list(g[1, modsC, drop = FALSE]),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  
  # strict column normalization to avoid match.names issues before binding
  #make sure verything has the same columns in the same order
  cols <- c("Term", modsC)
  rows <- lapply(rows, function(d) {
    miss <- setdiff(cols, names(d))
    # fill any missing model columns
    if (length(miss)) d[miss] <- "" 
    # enforce column order
    d <- d[, cols, drop = FALSE]  
    # keep everything as character
    d[] <- lapply(d, as.character)              
    d
  })
  
  #bind everything into one pretty table
  pretty <- do.call(rbind, rows)
  rownames(pretty) <- NULL
  colnames(pretty) <- cols
  #return table and first gof lab for formatting later on
  list(
    df = pretty, 
    gof_first = if (nrow(gof)) gof$term[1] else NA_character_
  )
}