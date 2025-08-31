# R/report_word.R

.report_docx <- function(res, out) {
  roles <- tryCatch(res$roles_df, error = function(e) NULL)
  mods  <- tryCatch(res$models,    error = function(e) NULL)
  msets <- tryCatch(res$min_sets,  error = function(e) list())
  canon <- tryCatch(res$canon,     error = function(e) character(0))
  
  md <- c("# DAGassist Report", "")
  
  if (is.data.frame(roles) && nrow(roles)) {
    rp <- .roles_pretty(roles)  # keep your existing roles prettyfier
    md <- c(md, "## Roles", "", .df_to_md_pipe(rp), "")
  }
  
  md <- c(md, "## Models", "", .msummary_to_markdown(mods), "")
  
  md <- c(md, paste0("*Controls (minimal):* ",
                     if (length(msets)) .set_brace_plain(msets[[1]]) else "{}"),
          paste0("*Controls (canonical):* ", .set_brace_plain(canon), "."),
          ""
  )
  
  .pandoc_docx(md, out)
}