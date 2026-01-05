# dotwhisker.R
# Internal helpers to render a dot-and-whisker plot for DAGassist outputs
#' @keywords internal
#' @importFrom stats setNames
utils::globalVariables(c(".data", "model"))

.report_dotwhisker <- function(report, out = NULL, width = 8, height = 6, dpi = 300) {
  .dw_require(c("dotwhisker", "broom", "dplyr", "ggplot2"))
  
  mods <- .build_named_mods(report)  # grab models from assist
  
  # tidy and attach model labels
  omit_re <- report$settings$coef_omit
  labmap  <- report$labels_map
  
  td_list <- lapply(names(mods), function(nm) {
    m <- mods[[nm]]
    out <- tryCatch(broom::tidy(m, conf.int = TRUE), error = function(e) NULL)
    if (is.null(out)) return(NULL)
    
    ## keep the treatment coefficient
    # reworked to handle factor treatments
    exp <- get_by_role(report$roles, "exposure")
    out <- .dw_keep_exposure_coef(out, exp)
    if (nrow(out) == 0) return(NULL)
    
    out$model <- nm
    out
  })
  td <- dplyr::bind_rows(td_list)
  if (!nrow(td)) stop("No estimable models available for dotwhisker.", call. = FALSE)
  
  if (!is.null(omit_re)) {
    td <- td[!grepl(omit_re, td$term), , drop = FALSE]
  }
  if (length(labmap)) {
    td$term <- ifelse(td$term %in% names(labmap), unname(labmap[td$term]), td$term)
  }
  
  # fix model order -- original, minimal, canonical
  mod_levels <- names(mods)
  td$model <- factor(td$model, levels = mod_levels)
  
  # build the plot. map color, shape, line type all to model
  dodge_size <- 0.65
  p <- dotwhisker::dwplot(
    td,
    dodge_size = dodge_size,
    vline = ggplot2::geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
    #include colour=model on both layers so they share one legend. otherwise, 
    #there will be distinct legends for both color and shape
    dot_args = list(ggplot2::aes(shape = model,  colour = model)),
    whisker_args = list(ggplot2::aes(linetype = model, colour = model))
  ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.y  = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(color = "grey90"),
      legend.key.size = grid::unit(.2, "lines"),
      legend.key.height = grid::unit(.5, "line"),
      legend.justification = c(.02, .98),
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(x = "Estimate", y = NULL, title = "DAGassist Model Comparison")
  
  # align legend names
  k <- length(mod_levels)
  greys <- setNames(grDevices::gray.colors(k, start = 0.15, end = 0.70), mod_levels)
  
  shape_pool <- c(16, 17, 15, 3, 7, 8, 0, 1, 2, 4, 5, 6, 9, 10, 11, 12, 13, 14, 18)
  ltype_pool <- c("solid", "dashed", "dotdash", "twodash", "longdash", "dotted")
  
  shapes_nv <- setNames(rep_len(shape_pool, k), mod_levels)
  ltypes_nv <- setNames(rep_len(ltype_pool, k), mod_levels)
  
  p <- p +
    # single legend comes from color
    ggplot2::scale_color_manual(
      values = greys, name = "Model",
      breaks = mod_levels, labels = mod_levels, drop = FALSE
    ) +
    # keep shape/linetype in the plot but hide their guides
    ggplot2::scale_shape_manual(
      values = shapes_nv, guide = "none",
      breaks = mod_levels, limits = mod_levels, drop = FALSE
    ) +
    ggplot2::scale_linetype_manual(
      values = ltypes_nv, guide = "none",
      breaks = mod_levels, limits = mod_levels, drop = FALSE
    ) +
    # make the colour legend keys display the matching shapes/linetypes
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(
          shape    = unname(shapes_nv[mod_levels]),
          linetype = unname(ltypes_nv[mod_levels])
        )
      )
    )
  
  # save to file or print to console
  if (!is.null(out)) {
    ggplot2::ggsave(filename = out, plot = p, width = width, height = height, dpi = dpi)
  } else {
    print(p)
  }
  invisible(p)
}


#dependency checker
.dw_require <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop(
      "For dotwhisker output, please install: ", paste(miss, collapse = ", "),
      " (e.g., install.packages(c(", paste0('"', miss, '"', collapse = ","), "))).",
      call. = FALSE
    )
  }
}

#helper for handling factor treatments
.dw_keep_exposure_coef <- function(td, exposure) {
  if (is.null(td) || !nrow(td) || is.na(exposure) || !nzchar(exposure)) return(td)
  
  # exact match 
  if (any(td$term == exposure)) {
    return(td[td$term == exposure, , drop = FALSE])
  }
  
  # factors typically start with the variable name
  esc <- gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", exposure)
  idx <- grepl(paste0("^", esc), td$term) & !grepl(":", td$term, fixed = TRUE)
  
  if (sum(idx) == 1) return(td[idx, , drop = FALSE])
  if (sum(idx) > 1) {
    cand <- td[idx, , drop = FALSE]
    cand <- cand[order(nchar(cand$term)), , drop = FALSE]
    return(cand[1, , drop = FALSE])
  }
  
  #last resort: any term containing exposure (prefer non-interactions)
  idx2 <- grepl(exposure, td$term, fixed = TRUE)
  if (sum(idx2) == 1) return(td[idx2, , drop = FALSE])
  if (sum(idx2) > 1) {
    cand <- td[idx2, , drop = FALSE]
    cand_no_int <- cand[!grepl(":", cand$term, fixed = TRUE), , drop = FALSE]
    if (nrow(cand_no_int)) cand <- cand_no_int
    cand <- cand[order(nchar(cand$term)), , drop = FALSE]
    return(cand[1, , drop = FALSE])
  }
  
  td[0, , drop = FALSE]
}