# Generated finance explorer app runtime snapshot
suppressPackageStartupMessages({
  library(tidyverse)
})

Sys.setenv(VROOM_THREADS = "1")

analysis_root <- normalizePath('.', winslash = '/', mustWork = TRUE)
finance_explorer_layer_dir <- fs::path(analysis_root, 'R', 'runtime', 'finance_explorer')
finance_explorer_report_path <- fs::path(analysis_root, 'README.md')
finance_explorer_env <- new.env(parent = emptyenv())
finance_app_bundle_path <- fs::path(analysis_root, 'data', 'finance_app_bundle.rds')

finance_explorer_layer_files <- c(
  'finance_explorer_01_request_catalog.R',
  'finance_explorer_02_context_base.R',
  'finance_explorer_03_assemble_rank.R',
  'finance_explorer_04_plot.R'
)

purrr::walk(
  finance_explorer_layer_files,
  \(file_name) {
    source(fs::path(finance_explorer_layer_dir, file_name), local = FALSE)
  }
)

load_finance_app_bundle <- function(bundle_path = finance_app_bundle_path) {
  readRDS(bundle_path)
}

load_finance_explorer_context <- function(refresh = FALSE,
                                          bundle_path = finance_app_bundle_path) {
  if (!refresh && exists('context', envir = finance_explorer_env, inherits = FALSE)) {
    return(get('context', envir = finance_explorer_env, inherits = FALSE))
  }

  context <- load_finance_app_bundle(bundle_path)
  assign('context', context, envir = finance_explorer_env)
  context
}
