# Load readable analysis tools and keep the study's date fixed.
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
options(timeout = 180, readr.show_col_types = FALSE)
config <- list(
  as_of = as.Date(Sys.getenv("ANALYSIS_AS_OF", "2026-10-26")),
  start_fy = 2018L,
  sample_size = if ("--full" %in% commandArgs(TRUE)) Inf else 20L,
  user_agent = "Haoqi Luo haoqi.luo23@hoosac.org",
  sec_pause = 0.22
)
for (folder in c("data/raw/submissions", "data/raw/filings", "data/raw/companyfacts",
                 "data/raw/prices", "data/processed", "output/figures", "output/tables")) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

# Record source problems instead of silently dropping companies.
record_issue <- function(stage, ticker, item, reason, detail = "") {
  if (Sys.getenv("AI_CACHE_ONLY", "0") == "1") {
    message(stage, " | ", ticker, " | ", reason, " | ", detail)
    return(invisible(NULL))
  }
  row <- tibble(stage, ticker, item = as.character(item), reason, detail)
  file <- "output/tables/data_issues.csv"
  old <- if (file.exists(file)) read_csv(file, col_types = cols(.default = "c")) else row[0, ]
  write_csv(distinct(bind_rows(old, row)), file)
  message(stage, " | ", ticker, " | ", reason, " | ", detail)
}

# Save successful source responses with their original retrieval metadata.
save_response <- function(response, url, file) {
  body <- httr2::resp_body_raw(response)
  if (!length(body)) stop("Empty response: ", url)
  temporary <- paste0(file, ".tmp")
  if (str_ends(file, "\\.gz")) {
    connection <- gzfile(temporary, "wb")
    writeBin(body, connection)
    close(connection)
  } else writeBin(body, temporary)
  if (!file.rename(temporary, file)) stop("Could not save cache: ", file)
  manifest <- tibble(url, file, retrieved_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
                     bytes = file.info(file)$size, md5 = unname(tools::md5sum(file)))
  write_csv(manifest, "data/raw/download_manifest.csv", append = file.exists("data/raw/download_manifest.csv"))
  file
}

# Share a four-request-per-second throttle, including retries and parallel transfers.
source_request <- function(url, sec = TRUE) {
  request <- httr2::request(url) |> httr2::req_user_agent(config$user_agent) |>
    httr2::req_options(accept_encoding = "gzip, deflate") |>
    httr2::req_timeout(120) |> httr2::req_retry(max_tries = 3)
  if (sec) request <- httr2::req_throttle(request, rate = 4,
                                        fill_time_s = 0.25, realm = "sec_edgar")
  request
}
fetch_cache <- function(url, file, sec = TRUE) {
  if (file.exists(file) && file.info(file)$size > 0) return(file)
  response <- httr2::req_perform(source_request(url, sec))
  save_response(response, url, file)
}

# Read cached JSON and classify SIC codes using the official broad divisions.
read_json_cache <- function(file) jsonlite::fromJSON(file, simplifyVector = TRUE)
sic_industry <- function(sic) {
  case_when(
    between(sic, 100, 999) ~ "Agriculture",
    between(sic, 1000, 1499) ~ "Mining",
    between(sic, 1500, 1799) ~ "Construction",
    between(sic, 2000, 3999) ~ "Manufacturing",
    between(sic, 4000, 4999) ~ "Transportation & utilities",
    between(sic, 5000, 5199) ~ "Wholesale trade",
    between(sic, 5200, 5999) ~ "Retail trade",
    between(sic, 6000, 6799) ~ "Finance, insurance & real estate",
    between(sic, 7000, 8999) ~ "Services",
    between(sic, 9100, 9729) ~ "Public administration",
    TRUE ~ "Unclassified"
  )
}
