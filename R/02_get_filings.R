# Follow the submissions history links so earlier 10-K filings are included.
companies <- read_csv("data/processed/companies.csv", col_types = cols(cik = "c"))
filing_rows <- list()
company_rows <- list()
for (i in seq_len(nrow(companies))) {
  company_row <- companies[i, ]
  ticker <- company_row$ticker
  cik <- company_row$cik
  sec_cik <- company_row$sec_cik
  message("Filings ", i, "/", nrow(companies), ": ", ticker)
  tryCatch({
    file <- fetch_cache(paste0("https://data.sec.gov/submissions/CIK", sec_cik, ".json"),
                        paste0("data/raw/submissions/CIK", sec_cik, ".json"))
    submission <- read_json_cache(file)
    sic <- as.integer(submission$sic)
    company_rows[[i]] <- company_row |> mutate(sic = sic, industry = sic_industry(sic))
    history <- as_tibble(submission$filings$recent)
    old_files <- as_tibble(submission$filings$files)
    if (nrow(old_files)) {
      old_files <- old_files |> filter(as.Date(filingTo) >= as.Date("2018-01-01"))
      for (history_name in old_files$name) {
        old_file <- fetch_cache(paste0("https://data.sec.gov/submissions/", history_name),
                                paste0("data/raw/submissions/", history_name))
        history <- bind_rows(history, as_tibble(read_json_cache(old_file)))
      }
    }
    filings <- history |> filter(form == "10-K") |>
      mutate(filing_date = as.Date(filingDate), period_end = as.Date(reportDate)) |>
      filter(period_end >= as.Date("2018-01-01"), filing_date <= config$as_of) |>
      distinct(accessionNumber, .keep_all = TRUE) |>
      arrange(period_end, filing_date) |>
      group_by(period_end) |> slice(1) |> ungroup() |>
      transmute(cik, sec_cik, ticker, company = company_row$company, sic, industry = sic_industry(sic),
                accession = accessionNumber, filing_date, period_end,
                primary_document = primaryDocument,
                url = paste0("https://www.sec.gov/Archives/edgar/data/", as.integer(sec_cik), "/",
                             str_remove_all(accessionNumber, "-"), "/", primaryDocument),
                local_file = paste0("data/raw/filings/", sec_cik, "_", accessionNumber, ".html.gz"))
    if (!nrow(filings)) record_issue("filings", ticker, cik, "no_eligible_10k", "No annual 10-K with period end from 2018 onward.")
    filings$download_status <- if_else(file.exists(filings$local_file), "ok", "pending")
    filing_rows[[i]] <- filings
  }, error = function(e) {
    if (str_detect(conditionMessage(e), "Downloads interrupted")) stop(e)
    record_issue("filings", ticker, cik, "submissions_failed", conditionMessage(e))
  })
  write_csv(bind_rows(filing_rows), "data/processed/filings.csv")
  write_csv(bind_rows(company_rows), "data/processed/companies_with_sic.csv")
}
if (!nrow(bind_rows(filing_rows))) stop("No filings acquired. See data_issues.csv.")
if (nrow(bind_rows(company_rows)) < 0.9 * nrow(companies)) {
  stop("Submissions access failed for over 10% of the selected issuers. Resolve access before analyzing a partial full run.")
}

# Download small batches across issuers while keeping request starts under the shared SEC limit.
filings <- bind_rows(filing_rows)
pending <- which(filings$download_status == "pending")
batches <- split(pending, ceiling(seq_along(pending) / 40))
for (batch_number in seq_along(batches)) {
  batch <- batches[[batch_number]]
  message("Document batch ", batch_number, "/", length(batches), ": ", length(batch), " uncached reports")
  responses <- httr2::req_perform_parallel(map(filings$url[batch], source_request),
    max_active = 5, on_error = "return", progress = FALSE)
  for (k in seq_along(batch)) {
    j <- batch[k]
    if (inherits(responses[[k]], "httr2_response")) {
      save_response(responses[[k]], filings$url[j], filings$local_file[j])
      filings$download_status[j] <- "ok"
    } else if (is.null(responses[[k]])) {
      stop("Downloads interrupted; rerun the stage to reuse completed caches.")
    } else {
      filings$download_status[j] <- "download_failed"
      record_issue("filings", filings$ticker[j], filings$accession[j], "download_failed",
                   conditionMessage(responses[[k]]))
    }
  }
  write_csv(filings, "data/processed/filings.csv")
  rm(responses)
}
