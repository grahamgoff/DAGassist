# tests/testthat/test-estimand.R

# ---- normalization -----------------------------------------------------------

test_that("every requested estimand survives normalization", {
  # regression test: a multi-estimand request must not silently drop members
  expect_setequal(.dagassist_normalize_estimand(c("total", "direct")),
                  c("TOTAL", "DIRECT"))
  expect_identical(.dagassist_normalize_estimand("total"),  "TOTAL")
  expect_identical(.dagassist_normalize_estimand("direct"), "DIRECT")
})

test_that("normalization is case-insensitive and defaults to RAW", {
  expect_identical(.dagassist_normalize_estimand("TOTAL"), "TOTAL")
  expect_identical(.dagassist_normalize_estimand("Total"), "TOTAL")
  expect_identical(.dagassist_normalize_estimand(NULL),    "RAW")
  expect_identical(.dagassist_normalize_estimand("none"),  "RAW")
})

test_that("an unknown estimand errors instead of being dropped", {
  expect_error(.dagassist_normalize_estimand("bogus"))
  # the dangerous case: one valid + one typo must not silently succeed
  expect_error(.dagassist_normalize_estimand(c("total", "bogus")))
})

# ---- display names -----------------------------------------------------------

test_that(".dagassist_display_names maps internal names to display labels", {
  expect_identical(
    .dagassist_display_names(c("Original", "Minimal 1", "Canonical",
                               "Minimal 1 (total)", "Canonical (total)",
                               "Direct (Raw)", "Direct (Weighted)")),
    c("Original", "Total Minimal 1 (Raw)", "Total Canonical (Raw)",
      "Total Minimal 1 (Weighted)", "Total Canonical (Weighted)",
      "Direct (Raw)", "Direct (Weighted)")
  )
})

test_that("display names leave unrelated columns untouched", {
  nms <- c("Original", "Bivariate", "Canon. (-NCT)")
  expect_identical(.dagassist_display_names(nms), nms)
})

# ---- column construction and ordering ----------------------------------------

test_that("default estimand leaves column names untouched", {
  skip_if_no_dagitty()
  dat <- sim_data_estimand()
  rpt <- DAGassist(make_dag_estimand(), lm(Y ~ X + Z, data = dat))
  nms <- names(.build_named_mods(rpt))
  
  expect_true("Canonical" %in% nms)
  expect_false(any(grepl("^Total ", nms)))
})

test_that("estimand = 'total' alone produces weighted columns", {
  skip_if_no_dagitty(); skip_if_no_estimand_deps()
  dat <- sim_data_estimand()
  rpt <- DAGassist(make_dag_estimand(), lm(Y ~ X + Z, data = dat),
                   estimand = "total")
  nms <- names(.build_named_mods(rpt))
  
  expect_true(any(grepl("^Total .* \\(Weighted\\)$", nms)))
  expect_false(any(grepl("\\(TOTAL\\)", nms)))   # casing regression
})

test_that("both estimands produce grouped columns in report order", {
  skip_if_no_dagitty(); skip_if_no_estimand_deps()
  dat <- sim_data_estimand()
  rpt <- DAGassist(make_dag_estimand(), lm(Y ~ X + M + Z, data = dat),
                   estimand = c("total", "direct"))
  nms <- names(.build_named_mods(rpt))
  
  expect_identical(nms[1], "Original")
  expect_true(all(c("Total Canonical (Raw)", "Total Canonical (Weighted)",
                    "Direct (Raw)", "Direct (Weighted)") %in% nms))
  
  # grouping invariant: all Raw specs, then all Weighted, then the direct pair
  expect_lt(match("Total Canonical (Raw)",      nms),
            match("Total Canonical (Weighted)", nms))
  expect_lt(match("Total Canonical (Weighted)", nms),
            match("Direct (Raw)",               nms))
  expect_lt(match("Direct (Raw)", nms), match("Direct (Weighted)", nms))
})

test_that("'direct' errors on a DAG with no mediator", {
  skip_if_no_dagitty(); skip_if_no_estimand_deps()
  df <- sim_data_confounder(seed = 3)
  expect_error(
    DAGassist(make_dag_confounder(), lm(Y ~ X + Z, data = df),
              exposure = "X", outcome = "Y",
              estimand = "direct"),
    "mediator"
  )
})

# ---- diagnostics builders ----------------------------------------------------

test_that(".dagassist_weight_diagnostics_df returns one row per weighted model", {
  skip_if_no_dagitty(); skip_if_no_estimand_deps()
  dat  <- sim_data_estimand()
  rpt  <- DAGassist(make_dag_estimand(), lm(Y ~ X + Z, data = dat),
                    estimand = "total")
  wdf  <- .dagassist_weight_diagnostics_df(.build_named_mods(rpt))
  
  expect_s3_class(wdf, "data.frame")
  expect_true(all(c("model", "n", "w_min", "w_median", "w_max",
                    "ess", "ess_frac", "n_treated", "n_control",
                    "w_min_treated", "w_max_treated",
                    "w_min_control", "w_max_control", "flags") %in% names(wdf)))
  expect_true(all(grepl("^Total .* \\(Weighted\\)$", wdf$model)))
  expect_true(all(wdf$ess <= wdf$n, na.rm = TRUE))
  expect_true(all(wdf$w_min <= wdf$w_max, na.rm = TRUE))
})

test_that(".dagassist_weight_diagnostics_df is NULL when nothing is weighted", {
  expect_null(.dagassist_weight_diagnostics_df(list(Original = 1, Canonical = 2)))
  expect_null(.dagassist_weight_diagnostics_df(NULL))
})

test_that(".dagassist_balance_diagnostics_df returns tidy comparison rows", {
  skip_if_no_dagitty()
  df  <- sim_data_confounder(seed = 7)
  rpt <- DAGassist(make_dag_confounder(), Y ~ X + Z, df, "X", "Y")
  bdf <- .dagassist_balance_diagnostics_df(rpt)
  
  expect_s3_class(bdf, "data.frame")
  expect_true(all(c("reference", "comparison", "n_ref", "n_cmp",
                    "variable", "type", "smd", "flagged") %in% names(bdf)))
  expect_type(bdf$flagged, "logical")
  expect_true(all(bdf$type %in% c("binary", "continuous")))
})

test_that("balance builder returns NULL without data", {
  skip_if_no_dagitty()
  df  <- sim_data_confounder(seed = 7)
  rpt <- DAGassist(make_dag_confounder(), Y ~ X + Z, df, "X", "Y")
  rpt$.__data <- NULL
  expect_null(.dagassist_balance_diagnostics_df(rpt))
})

# ---- printers still consume the builders -------------------------------------

test_that("printers render from the shared builders", {
  skip_if_no_dagitty(); skip_if_no_estimand_deps()
  dat <- sim_data_estimand()
  rpt <- DAGassist(make_dag_estimand(), lm(Y ~ X + Z, data = dat),
                   estimand = "total")
  
  expect_output(.dagassist_print_weight_diagnostics(.build_named_mods(rpt)),
                "Weight diagnostics")
  expect_output(.dagassist_print_balance_diagnostics(rpt),
                "Balance diagnostics")
})

# ---- export formats ----------------------------------------------------------

test_that("diagnostics reach every non-console export format", {
  skip_if_no_dagitty(); skip_if_no_estimand_deps()
  skip_if_not_installed("modelsummary")
  
  dat <- sim_data_estimand()
  dir <- tempfile("dagassist-exports"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  
  call_one <- function(type, ext) {
    f <- file.path(dir, paste0("report", ext))
    DAGassist(make_dag_estimand(), lm(Y ~ X + M + Z, data = dat),
              estimand = c("total", "direct"), type = type, out = f)
    f
  }
  
  tex <- call_one("latex", ".tex")
  expect_true(file.exists(tex))
  txt_tex <- paste(readLines(tex, warn = FALSE), collapse = "\n")
  expect_match(txt_tex, "Balance diagnostics")
  expect_match(txt_tex, "Weight diagnostics")
  expect_match(txt_tex, "LOW\\\\_ESS|EXTREME\\\\_W|ESS")  # underscores escaped
  
  txtf <- call_one("text", ".md")
  expect_true(file.exists(txtf))
  txt_md <- paste(readLines(txtf, warn = FALSE), collapse = "\n")
  expect_match(txt_md, "Balance diagnostics")
  expect_match(txt_md, "Weight diagnostics")
  
  xlsx <- call_one("excel", ".xlsx")
  expect_true(file.exists(xlsx))
  skip_if_not_installed("readxl")
  expect_true(all(c("Balance", "Weights") %in% readxl::excel_sheets(xlsx)))
})

test_that("docx export renders when pandoc is available", {
  skip_if_no_dagitty(); skip_if_no_estimand_deps()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  
  dat <- sim_data_estimand()
  f <- tempfile(fileext = ".docx"); on.exit(unlink(f), add = TRUE)
  DAGassist(make_dag_estimand(), lm(Y ~ X + M + Z, data = dat),
            estimand = c("total", "direct"), type = "docx", out = f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
})