# Run from the repository root: pilot first, then the full issuer sample.
if (!file.exists("R/00_setup.R")) stop("Run this script from the repository root.")
source("R/00_setup.R")
stages <- c("01_get_companies", "02_get_filings", "03_count_ai_terms", "04_get_financials",
            "05_get_prices", "06_build_panel", "07_analysis")
arguments <- commandArgs(TRUE)
start_at <- if (any(str_detect(arguments, "^--from="))) as.integer(str_remove(arguments[str_detect(arguments, "^--from=")][1], "--from=")) else 1L
for (stage in stages[seq.int(start_at, length(stages))]) {
  message("\nRunning ", stage)
  source(paste0("R/", stage, ".R"))
}
writeLines(capture.output(sessionInfo()), "output/sessionInfo.txt")
if (file.exists("data/raw/download_manifest.csv")) {
  file.copy("data/raw/download_manifest.csv", "output/tables/source_manifest.csv", overwrite = TRUE)
}
if (file.exists("scripts/write_summary.R")) {
  source("scripts/write_summary.R")
  write_project_summaries()
}
