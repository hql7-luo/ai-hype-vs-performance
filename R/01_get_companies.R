# Try a live index list; never substitute tidyquant's stale built-in fallback.
membership_file <- "data/raw/sp500_membership.csv"
if (!file.exists(membership_file)) {
  live_index <- tryCatch(tidyquant::tq_index("SP500", use_fallback = FALSE),
                        error = function(e) {
                          record_issue("companies", "ALL", "SP500", "tq_index_failed", conditionMessage(e))
                          NULL
                        })
  wiki_file <- fetch_cache("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies",
                           "data/raw/sp500_wikipedia.html", sec = FALSE)
  wiki <- rvest::read_html(wiki_file) |> rvest::html_element("table#constituents") |>
    rvest::html_table()
  members <- wiki |> transmute(ticker = Symbol, company = Security,
                               cik = sprintf("%010d", as.integer(CIK)))
  if (!is.null(live_index) && nrow(live_index) >= 490) {
    index_tickers <- str_replace_all(live_index$symbol, fixed("-"), ".")
    if (!setequal(members$ticker, index_tickers)) {
      record_issue("companies", "ALL", "SP500", "membership_disagreement",
                   "Live tidyquant and Wikipedia differ; use the explicitly archived Wikipedia constituent table with CIKs.")
    }
  } else {
    record_issue("companies", "ALL", "SP500", "wikipedia_membership_used",
                 "Live tq_index unavailable or incomplete. Archived current Wikipedia table supplies membership and CIKs.")
  }
  stopifnot(nrow(members) >= 490, !anyNA(members$cik), !anyDuplicated(members$ticker))
  write_csv(members, membership_file)
  write_lines(c(paste("Retrieved:", config$as_of),
                "Membership source: https://en.wikipedia.org/wiki/List_of_S%26P_500_companies",
                "Current constituents; not historical membership. Survivorship bias applies."),
              "data/raw/membership_source.txt")
}

# Keep one alphabetically first share class per issuer and document discarded classes.
members <- read_csv(membership_file, col_types = cols(cik = "c")) |>
  arrange(cik, ticker) |> group_by(cik) |> mutate(share_class_rank = row_number()) |> ungroup()
write_csv(members, "data/processed/membership_snapshot.csv")
write_csv(filter(members, share_class_rank > 1), "output/tables/share_class_exclusions.csv")
companies <- members |> filter(share_class_rank == 1) |> select(-share_class_rank)
# Apply only explicitly verified predecessor mappings, preserving today's issuer CIK as the key.
overrides <- read_csv("config/cik_history_overrides.csv", col_types = cols(current_cik = "c", sec_cik = "c"))
if (any(config$as_of > as.Date(overrides$valid_through))) stop("Review expired CIK history overrides before refreshing the study.")
companies <- companies |> left_join(overrides |> select(cik = current_cik, sec_cik), by = "cik") |>
  mutate(sec_cik = coalesce(sec_cik, cik))
if (is.finite(config$sample_size)) {
  pilot <- c("AAPL", "MSFT", "NVDA", "GOOG", "AMZN", "META", "IBM", "CRM", "ORCL", "ADBE",
             "JPM", "BAC", "BRK.B", "UNH", "JNJ", "WMT", "XOM", "CAT", "NEE", "PLD")
  companies <- companies |> filter(ticker %in% pilot)
  stopifnot(nrow(companies) == 20)
}
write_csv(companies, "data/processed/companies.csv")
message("Selected ", nrow(companies), " unique issuers from ", nrow(members), " constituent listings.")
